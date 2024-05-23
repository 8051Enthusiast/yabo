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
#include "executor.hpp"
#include "filecontent.hpp"
#include "filerequester.hpp"
#include "graph.hpp"
#include "request.hpp"
#include "yabo.hpp"
#include "yabo/vtable.h"

TreeIndex Arborist::add_node(ParentBranch parent, QString &field_name) {
  auto node = TreeNode{parent, TreeNodeState::LOADING, 0, field_name, {}, {}};
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
  executor_thread = new QThread();
  Executor *executor;
  this->file = file;
  executor = new Executor(path, file);
  executor->moveToThread(executor_thread);
  arborist = std::make_unique<Arborist>();
  connect(executor_thread, &QThread::finished, executor, &QObject::deleteLater);
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
    node.continuation = val;
    // idx.idx == 0 so that the top root is loaded when it is first seen
    // so that we can expand it automatically
    if ((recursive_fetch && root_info.at(root.root_idx).visited) ||
        idx.idx == 0) {
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
  root_info.push_back({val, generate_new_node_color(val), false});
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
    auto &vals = std::get<ValVecResponse>(resp.data);
    if (vals.vals.empty()) {
      arborist->get_node(resp.metadata.idx).state = TreeNodeState::LOADED;
      break;
    }
    auto continuation = vals.continuation;
    auto &val_vec = vals.vals;
    auto start = arborist->get_node(resp.metadata.idx).n_children;
    emit tree_begin_insert_rows(resp.metadata.idx, start,
                                start + val_vec.size() - 1, resp.metadata.root);
    for (size_t i = 0; i < val_vec.size(); i++) {
      auto branch = ParentBranch{resp.metadata.idx, i + start};
      auto tree_idx = arborist->add_node(branch, val_vec[i].name);
      set_value(tree_idx, val_vec[i].val, resp.metadata.root);
    }
    auto &node = arborist->get_node(resp.metadata.idx);
    node.n_children += val_vec.size();
    if (continuation) {
      node.state = TreeNodeState::LOADED_INCOMPLETE_CHIlDREN;
    } else {
      node.state = TreeNodeState::LOADED;
    }
    node.continuation = continuation;
    emit tree_end_insert_rows(resp.metadata.idx, resp.metadata.root);
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
  if (!inner_val.active) {
    return FileSpan();
  } else {
    return inner_val.span;
  }
}

void FileRequester::init_root(QString parser_name) {
  auto idx = arborist->add_root_node(root_count++, parser_name);
  root_info.push_back(
      {parser_name, generate_new_node_color(parser_name, 0), true});
  parser_root.insert({{parser_name, 0}, idx});
  graph_update.new_components.push_back(Node{idx});
  emit update_graph(std::move(graph_update));
  graph_update.new_components = {};
  emit parse_request(Meta{idx.tree_index, MessageType::PARSE, idx}, parser_name,
                     0);
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
  auto val = SpannedVal{*node.continuation, node.val->span, node.val->active};
  auto start = node.n_children;
  MessageType message_type;
  switch (val.kind()) {
  case YaboValKind::YABOARRAY:
    message_type = MessageType::ARRAY_ELEMENTS;
    break;
  case YaboValKind::YABOBLOCK:
    if (val.is_list_block()) {
      message_type = MessageType::ARRAY_ELEMENTS;
    } else {
      message_type = MessageType::FIELDS;
    }
    break;
  default:
    return;
  }
  node.state = TreeNodeState::LOADING_CHILDREN;
  emit request(Request{Meta{idx, message_type, root}, val, start});
}

std::optional<QColor> FileRequester::color(TreeIndex idx) const {
  auto &node = arborist->get_node(idx);
  if (node.val.has_value() && node.val->kind() == YaboValKind::YABONOM) {
    auto bubble = nominal_bubbles.at(node.val.value());
    return node_color(bubble);
  }
  return {};
}

void FileRequester::set_parser(QString name, size_t pos) {
  auto it = parser_root.find({name, pos});
  if (it != parser_root.end()) {
    change_root(it->second);
    return;
  }
  auto idx = arborist->add_root_node(root_count++, name);
  root_info.push_back({name, generate_new_node_color(name, pos)});
  parser_root.insert({{name, pos}, idx});
  graph_update.new_components.push_back(Node{idx});
  emit update_graph(std::move(graph_update));
  graph_update.new_components = {};
  emit parse_request(Meta{idx.tree_index, MessageType::PARSE, idx}, name, pos);
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
  root_info.at(root.root_idx).visited = true;
  emit select_range(0, 0);
  emit root_changed(Node(root));
}

std::unique_ptr<FileRequester> FileRequesterFactory::create_file_requester(
    QString parser_lib_path, QString file_path, QString parser_name,
    bool recursive_fetch) {
  try {
    FileRef file = std::make_shared<FileContent>(file_path.toStdString());
    return create_file_requester(parser_lib_path, file, parser_name,
                                 recursive_fetch);
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
  }
}

std::unique_ptr<FileRequester>
FileRequesterFactory::create_file_requester(QString parser_lib_path,
                                            FileRef file, QString parser_name,
                                            bool recursive_fetch) {
  std::filesystem::path p = parser_lib_path.toStdString();
  try {
    auto req =
        std::make_unique<FileRequester>(p, file, parser_name, recursive_fetch);
    return req;
  } catch (ExecutorError &e) {
    auto msg = std::stringstream() << "Could not open file: " << e.what();
    auto req =
        std::make_unique<FileRequester>(QString::fromStdString(msg.str()));
    return req;
  }
}

QString FileRequester::node_name(Node idx) const {
  auto cause = root_info.at(idx.idx).cause;
  if (std::holds_alternative<QString>(cause)) {
    return std::get<QString>(cause);
  }
  auto val = std::get<YaboVal>(cause);
  auto name = reinterpret_cast<NominalVTable *>(val->vtable)->name;
  return QString::fromUtf8(name);
}

QColor FileRequester::node_color(Node idx) const {
  return root_info.at(idx.idx).color;
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

QColor FileRequester::generate_new_node_color(QString val, size_t pos) const {
  size_t hash = std::hash<QString>()(val);
  hash ^= std::hash<size_t>()(pos);
  return random_color(hash);
}

void FileRequester::select_idx(TreeIndex idx) {
  auto sp = span(idx);
  if (!sp.data()) {
    return;
  }
  auto start = sp.data() - file->span().data();
  auto end = start + sp.size();
  emit select_range(start, end);
}
