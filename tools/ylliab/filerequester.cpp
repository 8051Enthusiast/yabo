#include <cstdio>
#include <filesystem>
#include <fstream>
#include <random>

#include <qthread.h>
#include <qvariant.h>
#include <vector>

#include "filerequester.hpp"
#include "request.hpp"
#include "yabo.hpp"
#include "yabo/vtable.h"
#include "yabotreemodel.hpp"

Executor::Executor(std::filesystem::path path, std::vector<uint8_t> &&file)
    : file(std::move(file)) {
  // we need to create a tmpfile copy of the library pointed to by
  // `path` which we then dlopen
  // this is because dlopen does not work well if the file changes, and
  // global symbols would get deduplicated which is also a bad idea
  try {
    auto tmp_root = std::filesystem::temp_directory_path();
    // note: this is essentially tmpnam, but we are cool and totally
    // allowed to do this
    // (an attacker can just mess with the file after it was created and
    // before we dlopen it anyway, and we can only dlopen through
    // a file path)
    auto random_num = std::random_device()();
    tmp_file = tmp_root / std::format("yabo_{:x}.so", random_num);

    std::filesystem::copy_file(
        path, tmp_file, std::filesystem::copy_options::overwrite_existing);
  } catch (std::filesystem::filesystem_error &e) {
    auto err = std::format("Could not create temporary file: {}", e.what());
    throw ExecutorError(err);
  }

  lib = dlopen(tmp_file.c_str(), RTLD_LAZY);
  if (!lib) {
    auto err =
        std::format("Could not open file {}; {}", path.string(), dlerror());
    throw ExecutorError(err);
  }

  auto size = reinterpret_cast<size_t *>(dlsym(lib, "yabo_max_buf_size"));
  if (!size) {
    auto err = std::format("File does not contain yabo_ma_buf_size symbol: {}. "
                           "Is the file in the right format?",
                           dlerror());
    throw ExecutorError(err);
  }
  vals = YaboValCreator(YaboValStorage(*size));
}

Executor::~Executor() {
  dlclose(lib);
  std::filesystem::remove(tmp_file);
}

std::optional<Response> Executor::get_fields(Request &req) {
  std::vector<std::pair<std::string, SpannedVal>> ret;
  auto vtable = reinterpret_cast<BlockVTable *>(req.val.val->vtable);
  for (size_t i = 0; i < vtable->fields->number_fields; i++) {
    auto name = vtable->fields->fields[i];
    auto field_val = vals.access_field(req.val, name);
    if (field_val.has_value()) {
      auto new_val = normalize(field_val.value(), req.val.span);
      ret.push_back(std::pair(name, new_val));
    }
  }
  return Response(req.metadata, std::move(ret));
}

std::optional<Response> Executor::get_array_members(Request &req) {
  std::vector<std::pair<std::string, SpannedVal>> ret;
  auto len = vals.array_len(req.val);
  for (size_t i = 0; i < len; i++) {
    auto idx_val = vals.index(req.val, i);
    if (idx_val.has_value()) {
      auto new_val = normalize(idx_val.value(), req.val.span);
      ret.push_back(std::pair(std::to_string(i), new_val));
    } else {
      return {};
    }
  }
  return Response(req.metadata, std::move(ret));
}

std::optional<Response> Executor::execute_request(Request req) {
  switch (req.metadata.kind) {
  case MessageType::FIELDS: {
    return get_fields(req);
  }
  case MessageType::ARRAY_ELEMENTS: {
    return get_array_members(req);
  }
  case MessageType::DEREF: {
    auto normalized = normalize(req.val, req.val.span);
    return Response(req.metadata, normalized);
  }
  case MessageType::PARSE:
  case MessageType::ERROR: {
    // parse requests come in via the execute_parser slot
    return {};
  }
  default:
    return {};
  }
}

std::optional<Response> Executor::execute_parser(Meta meta,
                                                 char const *func_name) {
  auto parser_ptr = reinterpret_cast<ParseFun const *>(dlsym(lib, func_name));
  if (!parser_ptr) {
    return {};
  }
  auto parser = *parser_ptr;
  auto ret = vals.parse(parser, file);
  if (ret.has_value()) {
    auto normalized = normalize(ret.value(), ret->span);
    return Response(meta, normalized);
  }
  return {};
}

SpannedVal Executor::normalize(YaboVal val, FileSpan parent_span) {
  while (true) {
    auto ret = vals.deref(val);
    if (!ret.has_value()) {
      break;
    }
    // get the span before val is updated
    auto span = vals.extent(val);
    val = ret.value();
    if (span.has_value()) {
      parent_span = span.value();
    }
  }
  return SpannedVal{val, parent_span};
}

FileRequester::FileRequester(std::filesystem::path path,
                             std::vector<uint8_t> &&file) {
  executor_thread = std::make_unique<QThread>();
  Executor *executor;
  file_base = file.data();
  executor = new Executor(path, std::move(file));
  executor->moveToThread(executor_thread.get());
  arborist = std::make_unique<Arborist>();
  assert(connect(executor_thread.get(), &QThread::finished, executor,
                 &QObject::deleteLater));
  assert(connect(executor, &Executor::response, this,
                 &FileRequester::process_response));
  assert(connect(this, &FileRequester::request, executor,
                 &Executor::execute_request_slot));
  assert(connect(this, &FileRequester::parse_request, executor,
                 &Executor::execute_parser_slot));
  executor_thread->start();
}

void FileRequester::process_response(Response resp) {
  switch (resp.metadata.kind) {
  case MessageType::ARRAY_ELEMENTS:
  case MessageType::FIELDS: {
    auto tree_model = static_cast<YaboTreeModel *>(resp.metadata.user_data);
    auto &vals = std::get<YaboValVec>(resp.data);
    if (vals.empty()) {
      arborist->get_node(resp.metadata.idx).state = TreeNodeState::LOADED;
      break;
    }
    tree_model->begin_insert_rows(resp.metadata.idx, 0, vals.size() - 1);
    for (size_t i = 0; i < vals.size(); i++) {
      auto branch = ParentBranch{resp.metadata.idx, (int)i};
      auto tree_node = TreeNode{branch, TreeNodeState::LOADED_NO_CHIlDREN, 0,
                                std::move(vals[i].first), vals[i].second};
      arborist->add_node(tree_node);
    }
    arborist->get_node(resp.metadata.idx).n_children = vals.size();
    arborist->get_node(resp.metadata.idx).state = TreeNodeState::LOADED;
    tree_model->end_insert_rows();
    break;
  }
  case MessageType::ERROR:
    arborist->get_node(resp.metadata.idx).state = TreeNodeState::ERROR;
    break;
  case MessageType::PARSE:
  case MessageType::DEREF:
    auto val = std::get<SpannedVal>(resp.data);
    arborist->set_val(resp.metadata.idx, val);
    auto tree_model = static_cast<YaboTreeModel *>(resp.metadata.user_data);
    tree_model->data_changed(resp.metadata.idx);
    break;
  };
}

QVariant FileRequester::data(TreeIndex idx) const {
  auto state = arborist->get_node(idx).state;
  if (state == TreeNodeState::LOADING) {
    return QString();
  }
  if (state == TreeNodeState::ERROR) {
    return QString("Error");
  }

  auto val = arborist->get_node(idx).val;
  if (!val.has_value()) {
    return QString();
  }

  auto inner_val = val.value();

  switch (inner_val.kind()) {
  case YaboValKind::YABOERROR: {
    auto err = inner_val.access_error();
    if (err == BACKTRACK) {
      return QVariant("Backtrack");
    } else if (err == EOS) {
      return QVariant("EOS");
    } else {
      return QVariant("Exception");
    }
  }
  case YaboValKind::YABOINTEGER:
    return QVariant::fromValue(inner_val.access_int());
  case YaboValKind::YABOBIT:
    return QVariant::fromValue((bool)inner_val.access_bool());
  case YaboValKind::YABOCHAR:
    return QVariant::fromValue(inner_val.access_char());
  case YaboValKind::YABONOM:
  case YaboValKind::YABOU8:
    return QVariant("nominal");
  case YaboValKind::YABOPARSER:
    return QVariant("parser");
  case YaboValKind::YABOFUNARGS:
    return QVariant("function");
  case YaboValKind::YABOUNIT:
    return QVariant("unit");
  case YaboValKind::YABOARRAY:
    return QVariant("");
  case YaboValKind::YABOBLOCK:
    return QVariant("");
  default:
    return QVariant();
  }
}

FileSpan FileRequester::span(TreeIndex idx) const {
  auto &node = arborist->get_node(idx);
  auto val = node.val;
  if (!val.has_value()) {
    return FileSpan();
  }
  auto inner_val = val.value();
  auto parent = node.idx.parent;
  if (parent == INVALID_PARENT) {
    return inner_val.span;
  }
  auto parent_span = arborist->get_node(parent).val.value().span;
  if (parent_span.data() == inner_val.span.data() &&
      parent_span.size() == inner_val.span.size()) {
    return FileSpan();
  }
  return inner_val.span;
}

std::unique_ptr<YaboTreeModel>
FileRequester::create_tree_model(QString parser_name) {
  auto sparser_name = parser_name.toStdString();
  if (parser_root.contains(sparser_name)) {
    return std::make_unique<YaboTreeModel>(*this, std::move(sparser_name),
                                           parser_root[sparser_name]);
  }
  auto node = TreeNode{
      ParentBranch{INVALID_PARENT, 0}, TreeNodeState::LOADING, 0, "(root)", {}};
  auto idx = arborist->add_node(node);
  parser_root.insert({sparser_name, idx});
  auto tree_model =
      std::make_unique<YaboTreeModel>(*this, std::move(sparser_name), idx);
  // note: this is not safe if we allow changing tree models as the pointer
  // might get invalidated
  emit parse_request(Meta{idx, MessageType::PARSE, tree_model.get()},
                     parser_name);
  return tree_model;
}

bool FileRequester::can_fetch_children(TreeIndex idx) {
  auto node = arborist->get_node(idx);
  return node.state == TreeNodeState::LOADED_NO_CHIlDREN &&
         node.val.has_value() &&
         (node.val->kind() == YaboValKind::YABOARRAY ||
          node.val->kind() == YaboValKind::YABOBLOCK);
}

void FileRequester::fetch_children(TreeIndex idx, YaboTreeModel *tree_model) {
  auto &node = arborist->get_node(idx);
  if (node.state == TreeNodeState::LOADED_NO_CHIlDREN && node.val.has_value()) {
    auto val = node.val.value();
    MessageType message_type;
    switch (val.kind()) {
    case YaboValKind::YABOARRAY:
      message_type = MessageType::ARRAY_ELEMENTS;
      break;
    case YaboValKind::YABOBLOCK:
      message_type = MessageType::FIELDS;
      break;
    default:
      return;
    }
    node.state = TreeNodeState::LOADING_CHILDREN;
    emit request(Request{Meta{idx, message_type, tree_model}, val});
  }
}

std::unique_ptr<FileRequester>
FileRequesterFactory::create_file_requester(QString parser_lib_path,
                                            QString file_path) {
  std::filesystem::path p = parser_lib_path.toStdString();
  std::vector<uint8_t> file;
  try {
    std::ifstream f(file_path.toStdString(), std::ios::binary);
    auto file_size = std::filesystem::file_size(file_path.toStdString());
    file.resize(file_size);
    f.read((char *)file.data(), file_size);
    auto req = std::make_unique<FileRequester>(p, std::move(file));
    return req;
  } catch (std::system_error &e) {
    auto msg = std::format("Could not open file {}: {}",
                           file_path.toStdString(), e.what());
    auto req = std::make_unique<FileRequester>(QString::fromStdString(msg));
    return req;
  } catch (ExecutorError &e) {
    auto msg = std::format("Could not open file {}: {}",
                           file_path.toStdString(), e.what());
    auto req = std::make_unique<FileRequester>(QString::fromStdString(msg));
    return req;
  }
}
