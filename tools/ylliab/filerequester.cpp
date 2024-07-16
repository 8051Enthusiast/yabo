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

TreeIndex Arborist::add_node(TreeIndex parent, QString &field_name) {
  auto row = tree[parent.idx].children.size();
  auto idx = TreeIndex{tree.size()};
  auto branch = ParentBranch{parent, row};
  auto node = TreeNode{branch, {}, field_name, {}, {}, TreeNodeState::LOADING};
  tree.push_back(node);
  tree[parent.idx].children.push_back(idx);
  return idx;
}

RootIndex Arborist::add_root_node(QString &field_name, RootInfo info) {
  auto index = add_node(INVALID_PARENT, field_name);
  auto row = get_node(index).idx.row;
  assert(row == root_info.size());
  root_info.push_back(info);
  return RootIndex(index, row);
}

FileRequester::FileRequester(std::filesystem::path path, FileRef file,
                             bool recursive_fetch)
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
}

void FileRequester::set_value(TreeIndex idx, SpannedHandle val,
                              RootIndex root) {
  auto &node = arborist->get_node(idx);
  node.val = val;
  if (val.kind == YaboValKind::YABOARRAY ||
      val.kind == YaboValKind::YABOBLOCK) {
    node.state = TreeNodeState::LOADED_INCOMPLETE_CHIlDREN;
    node.continuation = val.access_val();
    if (recursive_fetch && arborist->get_root_info(root.root_idx).visited) {
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
  if (val.kind != YaboValKind::YABONOM) {
    return;
  }
  auto val_handle = val.access_val();
  assert(val_handle.has_value());
  auto handle = *val_handle;
  if (nominal_bubbles.contains(handle)) {
    auto end = nominal_bubbles.at(handle);
    graph_update.new_components.push_back(Edge{root, end});
    return;
  }
  auto empty = QString("");
  auto new_nominal_bubble = arborist->add_root_node(
      empty, {handle, val.name, generate_new_node_color(handle), false});
  auto req = Request{Meta{new_nominal_bubble.tree_index, MessageType::DEREF,
                          new_nominal_bubble},
                     val};
  nominal_bubbles.insert({handle, new_nominal_bubble});
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
    auto start = arborist->get_node(resp.metadata.idx).children.size();
    auto total = start + val_vec.size();
    arborist->get_node(resp.metadata.idx).children.reserve(total);
    emit tree_begin_insert_rows(resp.metadata.idx, start, total - 1,
                                resp.metadata.root);
    for (size_t i = 0; i < val_vec.size(); i++) {
      auto tree_idx = arborist->add_node(resp.metadata.idx, val_vec[i].name);
      set_value(tree_idx, val_vec[i].val, resp.metadata.root);
    }
    auto &node = arborist->get_node(resp.metadata.idx);
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

  switch (inner_val.kind) {
  case YaboValKind::YABOERROR: {
    auto err = *inner_val.access_error();
    if (err == YABO_STATUS_BACKTRACK) {
      return QVariant("Backtrack");
    } else if (err == YABO_STATUS_EOS) {
      return QVariant("EOF");
    } else {
      return QVariant("Exception");
    }
  }
  case YaboValKind::YABOINTEGER:
    return QVariant::fromValue(*inner_val.access_int());
  case YaboValKind::YABOBIT:
    return QVariant::fromValue(*inner_val.access_bool());
  case YaboValKind::YABOCHAR:
    return QVariant::fromValue(*inner_val.access_char());
  case YaboValKind::YABONOM: {
    return inner_val.name;
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

bool FileRequester::can_fetch_children(TreeIndex idx) {
  auto node = arborist->get_node(idx);
  return (node.state == TreeNodeState::LOADED_INCOMPLETE_CHIlDREN ||
          node.state == TreeNodeState::LOADING_CHILDREN) &&
         node.val.has_value() &&
         (node.val->kind == YaboValKind::YABOARRAY ||
          node.val->kind == YaboValKind::YABOBLOCK);
}

void FileRequester::fetch_children(TreeIndex idx, RootIndex root) {
  auto &node = arborist->get_node(idx);
  if (node.state != TreeNodeState::LOADED_INCOMPLETE_CHIlDREN ||
      !node.val.has_value()) {
    return;
  }
  auto val = SpannedHandle(*node.continuation, node.val->span, node.val->kind,
                           node.val->active, node.val->flags);
  auto start = node.children.size();
  MessageType message_type;
  switch (val.kind) {
  case YaboValKind::YABOARRAY:
    message_type = MessageType::ARRAY_ELEMENTS;
    break;
  case YaboValKind::YABOBLOCK:
    if (val.flags.is_list) {
      message_type = MessageType::ARRAY_ELEMENTS;
    } else {
      message_type = MessageType::FIELDS;
    }
    break;
  default:
    return;
  }
  if (idx == root.tree_index) {
    arborist->get_root_info(root.root_idx).visited = true;
  }
  node.state = TreeNodeState::LOADING_CHILDREN;
  emit request(Request{Meta{idx, message_type, root}, val, start});
}

std::optional<QColor> FileRequester::color(TreeIndex idx) const {
  auto &node = arborist->get_node(idx);
  if (node.val.has_value() && node.val->kind == YaboValKind::YABONOM) {
    auto bubble = nominal_bubbles.at(*node.val->access_val());
    return node_color(bubble);
  }
  return {};
}

RootIndex FileRequester::set_parser(QString name, size_t pos) {
  auto it = parser_root.find({name, pos});
  if (it != parser_root.end()) {
    return it->second;
  }
  auto idx = arborist->add_root_node(
      name, {{}, name, generate_new_node_color(name, pos)});
  parser_root.insert({{name, pos}, idx});
  graph_update.new_components.push_back(Node{idx});
  emit update_graph(std::move(graph_update));
  graph_update.new_components = {};
  emit parse_request(Meta{idx.tree_index, MessageType::PARSE, idx}, name, pos);
  return idx;
}

RootIndex FileRequester::root_idx(Node node) const {
  auto idx = arborist->get_child(INVALID_PARENT, node.idx);
  return RootIndex(idx, node.idx);
}

std::unique_ptr<FileRequester> FileRequesterFactory::create_file_requester(
    QString parser_lib_path, QString file_path, bool recursive_fetch) {
  try {
    FileRef file = std::make_shared<FileContent>(file_path.toStdString());
    return create_file_requester(parser_lib_path, file, recursive_fetch);
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

std::unique_ptr<FileRequester> FileRequesterFactory::create_file_requester(
    QString parser_lib_path, FileRef file, bool recursive_fetch) {
  std::filesystem::path p = parser_lib_path.toStdString();
  try {
    auto req = std::make_unique<FileRequester>(p, file, recursive_fetch);
    return req;
  } catch (ExecutorError &e) {
    auto msg = std::stringstream() << "Could not open file: " << e.what();
    auto req =
        std::make_unique<FileRequester>(QString::fromStdString(msg.str()));
    return req;
  }
}

QString FileRequester::node_name(Node idx) const {
  return arborist->get_root_info(idx.idx).name;
}

QColor FileRequester::node_color(Node idx) const {
  return arborist->get_root_info(idx.idx).color;
}

std::optional<std::pair<size_t, size_t>>
FileRequester::idx_range(TreeIndex idx) const {
  auto range = span(idx);
  if (!range.data()) {
    return {};
  }
  size_t start = range.data() - file->span().data();
  size_t end = start + range.size();
  return std::make_pair(start, end);
}

std::optional<std::pair<size_t, size_t>>
FileRequester::node_range(Node idx) const {
  auto ridx = root_idx(idx);
  return idx_range(ridx.tree_index);
}

QColor FileRequester::generate_new_node_color(ValHandle val) const {
  size_t hash = std::hash<ValHandle>()(val);
  return random_color(hash);
}

QColor FileRequester::generate_new_node_color(QString val, size_t pos) const {
  size_t hash = std::hash<QString>()(val);
  hash ^= std::hash<size_t>()(pos);
  return random_color(hash);
}

std::optional<Node> FileRequester::link(TreeIndex idx) const {
  auto &node = arborist->get_node(idx);
  if (node.val.has_value() && node.val->kind == YaboValKind::YABONOM) {
    auto bubble = nominal_bubbles.at(*node.val->access_val());
    return Node{bubble};
  }
  return {};
}
