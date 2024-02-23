#include <cstdio>
#include <filesystem>
#include <fstream>
#include <qcolor.h>
#include <qglobal.h>
#include <random>

#include <algorithm>
#include <qthread.h>
#include <qvariant.h>
#include <sstream>
#include <stdexcept>
#include <vector>

#include "color.hpp"
#include "filerequester.hpp"
#include "graph.hpp"
#include "request.hpp"
#include "yabo.hpp"
#include "yabo/vtable.h"

Executor::Executor(std::filesystem::path path, FileRef file_content)
    : file(file_content) {
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
    auto tmp_file_name =
        std::string("yabo_") + std::to_string(random_num) + std::string(".so");
    tmp_file = tmp_root / tmp_file_name;

    std::filesystem::copy_file(
        path, tmp_file, std::filesystem::copy_options::overwrite_existing);
  } catch (std::filesystem::filesystem_error &e) {
    auto err = std::stringstream()
               << "Could not create temporary file: " << e.what();
    throw ExecutorError(err.str());
  }

  lib = dlopen(tmp_file.c_str(), RTLD_LAZY);
  if (!lib) {
    auto err = std::stringstream()
               << "Could not open file " << path.string() << "; " << dlerror();
    std::filesystem::remove(tmp_file);
    throw ExecutorError(err.str());
  }

  auto size = reinterpret_cast<size_t *>(dlsym(lib, "yabo_max_buf_size"));
  if (!size) {
    auto err = std::stringstream()
               << "File does not contain yabo_ma_buf_size symbol: " << dlerror()
               << ". Is the file in the right format?";
    dlclose(lib);
    std::filesystem::remove(tmp_file);
    throw ExecutorError(err.str());
  }
  auto global_address =
      reinterpret_cast<Slice *>(dlsym(lib, "yabo_global_address"));
  if (global_address) {
    auto span = file->span();
    global_address->start = span.data();
    global_address->end = span.data() + span.size();
  }
  typedef int64_t (*init_fun)(void);
  auto global_init = reinterpret_cast<init_fun>(dlsym(lib, "yabo_global_init"));
  int64_t status = global_init();
  if (status) {
    auto err = std::stringstream()
               << "Global init failed with status " << status;
    dlclose(lib);
    std::filesystem::remove(tmp_file);
    throw ExecutorError(err.str());
  }
  vals = YaboValCreator(YaboValStorage(*size));
}

Executor::~Executor() {
  dlclose(lib);
  std::filesystem::remove(tmp_file);
}

std::optional<Response> Executor::get_fields(Request &req) {
  std::vector<NamedYaboVal> ret;
  auto vtable = reinterpret_cast<BlockVTable *>(req.val.val->vtable);
  for (size_t i = 0; i < vtable->fields->number_fields; i++) {
    auto name = vtable->fields->fields[i];
    auto field_val = vals.access_field(req.val, name);
    if (field_val.has_value()) {
      auto new_val = normalize(field_val.value(), req.val.span);
      ret.push_back(std::pair(QString(name), new_val));
    }
  }
  return Response(req.metadata, std::move(ret));
}

std::optional<Response> Executor::get_array_members(Request &req) {
  std::vector<NamedYaboVal> ret;
  uint64_t len = vals.array_len(req.val);
  uint64_t start = req.array_start_index;
  auto end = std::min(start + array_fetch_size, len);
  for (size_t i = start; i < end; i++) {
    auto idx_val = vals.index(req.val, i);
    if (idx_val.has_value()) {
      auto new_val = normalize(idx_val.value(), req.val.span);
      ret.push_back(std::pair(QString::number(i), new_val));
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
    auto name = reinterpret_cast<NominalVTable *>(req.val->vtable)->name;
    return Response(req.metadata, {name, normalized});
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
  auto ret = vals.parse(parser, file->span());
  if (ret.has_value()) {
    auto normalized = normalize(ret.value(), ret->span);
    return Response(meta, {func_name, normalized});
  }
  return {};
}

Executor::DerefInfo Executor::deref(YaboVal val) {
  auto it = deref_cache.find(val);
  if (it != deref_cache.end()) {
    return it->second;
  }
  auto ret = vals.deref(val);
  std::optional<FileSpan> span;
  if (ret.has_value()) {
    span = vals.extent(val);
  }
  auto info = DerefInfo{ret, span};
  deref_cache.insert({val, info});
  return info;
}

SpannedVal Executor::normalize(YaboVal val, FileSpan parent_span) {
  std::optional<SpannedVal> first_outside;
  while (true) {
    if (val.kind() == YaboValKind::YABOU8) {
      const uint8_t *start = val.access_u8();
      return SpannedVal{val, FileSpan(start, 1)};
    }
    auto deref_info = deref(val);
    if (!deref_info.val.has_value()) {
      break;
    }
    if (deref_info.span.has_value()) {
      if (!span_contains(parent_span, deref_info.span.value())) {
        first_outside = SpannedVal{val, deref_info.span.value()};
      }
      parent_span = deref_info.span.value();
    }
    val = deref_info.val.value();
  }
  if (first_outside.has_value() && (val.kind() == YaboValKind::YABOARRAY ||
                                    val.kind() == YaboValKind::YABOBLOCK)) {
    return first_outside.value();
  }
  return SpannedVal{val, parent_span};
}

TreeIndex Arborist::add_node(ParentBranch parent, QString &field_name) {
  auto node = TreeNode{parent, TreeNodeState::LOADING, 0, field_name, {}};
  tree.push_back(node);
  auto idx = TreeIndex{tree.size() - 1};
  interner.insert({node.idx, idx});
  return idx;
}

RootIndex Arborist::add_root_node(size_t root_count, QString &field_name) {
  auto index = add_node(ParentBranch{INVALID_PARENT, root_count}, field_name);
  return RootIndex(index, root_count);
}

FileRequester::FileRequester(std::filesystem::path path, FileRef file,
                             QString parser_name, bool recursive_fetch)
    : recursive_fetch(recursive_fetch) {
  Executor *executor;
  this->file = file;
  executor = new Executor(path, file);
  executor->moveToThread(&executor_thread);
  arborist = std::make_unique<Arborist>();
  connect(&executor_thread, &QThread::finished, executor,
          &QObject::deleteLater);
  connect(executor, &Executor::response, this,
          &FileRequester::process_response);
  connect(this, &FileRequester::request, executor,
          &Executor::execute_request_slot);
  connect(this, &FileRequester::parse_request, executor,
          &Executor::execute_parser_slot);
  init_root(parser_name);
}

void FileRequester::set_value(TreeIndex idx, SpannedVal val, RootIndex root) {
  auto &node = arborist->get_node(idx);
  node.val = val;
  node.n_children = 0;
  if (val.kind() == YaboValKind::YABOARRAY ||
      val.kind() == YaboValKind::YABOBLOCK) {
    node.state = TreeNodeState::LOADED_INCOMPLETE_CHIlDREN;
    if (recursive_fetch && root == current_root) {
      fetch_children(idx, root);
    }
  } else {
    node.state = TreeNodeState::LOADED;
  }
  if (idx == root.tree_index && val.span.data()) {
    auto node = Node{root};
    size_t start = val.span.data() - file->span().data();
    size_t end = start + val.span.size();
    emit new_node({start, end, node});
  }
  if (val.kind() != YaboValKind::YABONOM) {
    return;
  }
  if (nominal_bubbles.contains(val)) {
    auto end = nominal_bubbles.at(val);
    graph_update.new_components.push_back(Edge{root, end});
    return;
  }
  auto empty = QString("");
  auto new_nominal_bubble = arborist->add_root_node(root_count++, empty);
  root_causes.push_back({val, generate_new_node_color(val)});
  auto req = Request{Meta{new_nominal_bubble.tree_index, MessageType::DEREF,
                          new_nominal_bubble},
                     val};
  nominal_bubbles.insert({val, new_nominal_bubble});
  graph_update.new_components.push_back(
      EdgeWithNewNode{root, Node{new_nominal_bubble}});
  emit request(req);
}

void FileRequester::process_response(Response resp) {
  switch (resp.metadata.kind) {
  case MessageType::ARRAY_ELEMENTS:
  case MessageType::FIELDS: {
    auto &vals = std::get<YaboValVec>(resp.data);
    if (vals.empty()) {
      arborist->get_node(resp.metadata.idx).state = TreeNodeState::LOADED;
      break;
    }
    auto start = arborist->get_node(resp.metadata.idx).n_children;
    emit tree_begin_insert_rows(resp.metadata.idx, start,
                                start + vals.size() - 1, resp.metadata.root);
    for (size_t i = 0; i < vals.size(); i++) {
      auto branch = ParentBranch{resp.metadata.idx, i + start};
      auto tree_idx = arborist->add_node(branch, vals[i].first);
      set_value(tree_idx, vals[i].second, resp.metadata.root);
    }
    auto &node = arborist->get_node(resp.metadata.idx);
    node.n_children += vals.size();
    if (vals.size() == array_fetch_size &&
        resp.metadata.kind == MessageType::ARRAY_ELEMENTS) {
      node.state = TreeNodeState::LOADED_INCOMPLETE_CHIlDREN;
    } else {
      node.state = TreeNodeState::LOADED;
    }
    emit tree_end_insert_rows(resp.metadata.root);
    break;
  }
  case MessageType::ERROR:
    arborist->get_node(resp.metadata.idx).state = TreeNodeState::ERROR;
    break;
  case MessageType::PARSE:
  case MessageType::DEREF:
    auto [name, val] = std::get<NamedYaboVal>(resp.data);
    set_value(resp.metadata.idx, val, resp.metadata.root);
    auto &node = arborist->get_node(resp.metadata.idx);
    node.field_name = std::move(name);
    emit tree_data_changed(resp.metadata.idx, resp.metadata.root);
    break;
  };
  if (!graph_update.new_components.empty()) {
    emit update_graph(std::move(graph_update));
    graph_update.new_components = {};
  }
}

QVariant FileRequester::data(TreeIndex idx) const {
  auto state = arborist->get_node(idx).state;
  if (state == TreeNodeState::LOADING) {
    return QString("Loading...");
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
      return QVariant("EOF");
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
  case YaboValKind::YABONOM: {
    auto name = reinterpret_cast<NominalVTable *>(inner_val->vtable)->name;
    return QString::fromUtf8(name);
  }
  case YaboValKind::YABOU8: {
    // we special-case u8 to print hex
    uint8_t byte = *inner_val.access_u8();
    return QString::asprintf("0x%02x", byte);
  }
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

void FileRequester::init_root(QString parser_name) {
  auto idx = arborist->add_root_node(root_count++, parser_name);
  root_causes.push_back({parser_name, generate_new_node_color(parser_name)});
  parser_root.insert({parser_name, idx});
  graph_update.new_components.push_back(Node{idx});
  emit update_graph(std::move(graph_update));
  graph_update.new_components = {};
  emit parse_request(Meta{idx.tree_index, MessageType::PARSE, idx},
                     parser_name);
}

bool FileRequester::can_fetch_children(TreeIndex idx) {
  auto node = arborist->get_node(idx);
  return (node.state == TreeNodeState::LOADED_INCOMPLETE_CHIlDREN ||
          node.state == TreeNodeState::LOADING_CHILDREN) &&
         node.val.has_value() &&
         (node.val->kind() == YaboValKind::YABOARRAY ||
          node.val->kind() == YaboValKind::YABOBLOCK);
}

void FileRequester::fetch_children(TreeIndex idx, RootIndex root) {
  auto &node = arborist->get_node(idx);
  if (node.state != TreeNodeState::LOADED_INCOMPLETE_CHIlDREN ||
      !node.val.has_value()) {
    return;
  }
  auto val = node.val.value();
  auto start = node.n_children;
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
  emit request(Request{Meta{idx, message_type, root}, val, start});
}

QColor FileRequester::color(TreeIndex idx) const {
  auto &node = arborist->get_node(idx);
  if (node.val.has_value() && node.val->kind() == YaboValKind::YABONOM) {
    auto bubble = nominal_bubbles.at(node.val.value());
    return node_color(bubble);
  }
  return QColor(255, 255, 255);
}

void FileRequester::set_parser(QString name) {
  auto it = parser_root.find(name);
  if (it != parser_root.end()) {
    change_root(it->second);
    return;
  }
  auto idx = arborist->add_root_node(root_count++, name);
  root_causes.push_back({name, generate_new_node_color(name)});
  parser_root.insert({name, idx});
  graph_update.new_components.push_back(Node{idx});
  emit update_graph(std::move(graph_update));
  graph_update.new_components = {};
  emit parse_request(Meta{idx.tree_index, MessageType::PARSE, idx}, name);
  change_root(idx);
}

RootIndex FileRequester::root_idx(Node node) const {
  auto idx = arborist->get_child(INVALID_PARENT, node.idx);
  return RootIndex(idx, node.idx);
}

void FileRequester::set_bubble(TreeIndex idx) {
  auto &node = arborist->get_node(idx);
  if (!node.val.has_value()) {
    return;
  }
  if (node.val->kind() != YaboValKind::YABONOM) {
    return;
  }
  auto root = nominal_bubbles.at(node.val.value());
  change_root(root);
}

void FileRequester::change_root(Node node) { change_root(root_idx(node)); }

void FileRequester::change_root(RootIndex root) {
  if (root == current_root) {
    return;
  }
  current_root = root;
  emit root_changed(Node(root));
  if (recursive_fetch) {
    fetch_children(root.tree_index, root);
  }
}

std::unique_ptr<FileRequester> FileRequesterFactory::create_file_requester(
    QString parser_lib_path, QString file_path, QString parser_name,
    bool recursive_fetch) {
  std::filesystem::path p = parser_lib_path.toStdString();
  std::vector<uint8_t> file;
  try {
    FileRef file = std::make_shared<FileContent>(file_path.toStdString());
    auto req =
        std::make_unique<FileRequester>(p, file, parser_name, recursive_fetch);
    return req;
  } catch (std::system_error &e) {
    auto msg = std::stringstream()
               << "Could not open file " << file_path.toStdString() << ": "
               << e.what();
    auto req =
        std::make_unique<FileRequester>(QString::fromStdString(msg.str()));
    return req;
  } catch (std::runtime_error &e) {
    auto msg = std::stringstream()
               << "Could not open file " << file_path.toStdString() << ": "
               << e.what();
    auto req =
        std::make_unique<FileRequester>(QString::fromStdString(msg.str()));
    return req;
  } catch (ExecutorError &e) {
    auto msg = std::stringstream()
               << "Could not open file " << file_path.toStdString() << ": "
               << e.what();
    auto req =
        std::make_unique<FileRequester>(QString::fromStdString(msg.str()));
    return req;
  }
}

QString FileRequester::node_name(Node idx) const {
  auto cause = root_causes.at(idx.idx).cause;
  if (std::holds_alternative<QString>(cause)) {
    return std::get<QString>(cause);
  }
  auto val = std::get<YaboVal>(cause);
  auto name = reinterpret_cast<NominalVTable *>(val->vtable)->name;
  return QString::fromUtf8(name);
}

QColor FileRequester::node_color(Node idx) const {
  return root_causes.at(idx.idx).color;
}

std::optional<std::pair<size_t, size_t>>
FileRequester::node_range(Node idx) const {
  auto ridx = root_idx(idx);
  auto val = arborist->get_node(ridx.tree_index).val;
  if (!val) {
    return {};
  }
  auto span = val->span;
  if (!span.data()) {
    return {};
  }
  auto start = span.data() - file->span().data();
  auto end = start + span.size();
  return std::make_pair(start, end);
}

QColor FileRequester::generate_new_node_color(YaboVal val) const {
  size_t hash = std::hash<YaboVal>()(val);
  return random_color(hash);
}

QColor FileRequester::generate_new_node_color(QString val) const {
  size_t hash = std::hash<QString>()(val);
  return random_color(hash);
}
