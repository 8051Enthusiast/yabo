#pragma once
#include <filesystem>
#include <format>
#include <memory>
#include <system_error>

#include <QObject>
#include <qthread.h>
#include <qvariant.h>

#include <dlfcn.h>
#include <unordered_set>

#include "filecontent.hpp"
#include "graph.hpp"
#include "request.hpp"
#include "yabo.hpp"
#include "yabotreemodel.hpp"

struct ExecutorError : public std::exception {
  ExecutorError(std::string msg) : message(msg) {}
  std::string what() { return message; }
  std::string message;
};

class Executor : public QObject {
  Q_OBJECT
public:
  Executor(std::filesystem::path path, FileRef file);
  ~Executor();
  // we need to delete the copy constructor and assignment operator because
  // we don't want to copy the library
  Executor(const Executor &) = delete;
  Executor &operator=(const Executor &) = delete;
  Executor(Executor &&) = delete;
  Executor &operator=(Executor &&) = delete;

  std::optional<Response> execute_request(Request req);
  std::optional<Response> execute_parser(Meta meta, char const *func_name);
public slots:
  void execute_request_slot(Request req) {
    auto resp = execute_request(req);
    if (resp.has_value()) {
      emit response(std::move(resp.value()));
    } else {
      emit response(Response(req.metadata));
    }
  }
  void execute_parser_slot(Meta meta, QString func_name) {
    auto s = func_name.toStdString();
    auto resp = execute_parser(meta, s.c_str());
    if (resp.has_value()) {
      emit response(std::move(resp.value()));
    } else {
      emit response(Response(meta));
    }
  }
signals:
  void response(Response resp);

private:
  std::optional<Response> get_fields(Request &req);
  std::optional<Response> get_array_members(Request &req);
  SpannedVal normalize(YaboVal val, FileSpan parent_span);
  struct DerefInfo {
    std::optional<YaboVal> val;
    std::optional<FileSpan> span;
  };
  std::unordered_map<YaboVal, DerefInfo> deref_cache;
  DerefInfo deref(YaboVal val);
  YaboValCreator vals;
  void *lib;
  std::filesystem::path tmp_file;
  FileRef file;
};

struct ParentBranch {
  bool operator==(const ParentBranch &other) const noexcept {
    return parent == other.parent && row == other.row;
  }

  TreeIndex parent;
  size_t row;
};

// we need to hash TreeIndex for the cache
template <> struct std::hash<ParentBranch> {
  std::size_t operator()(const ParentBranch &k) const noexcept {
    return std::hash<size_t>()(k.parent.idx) + (std::hash<int>()(k.row) * 3);
  }
};

enum class TreeNodeState {
  LOADING,
  LOADED_NO_CHIlDREN,
  LOADING_CHILDREN,
  LOADED,
  ERROR,
};

struct TreeNode {
  ParentBranch idx;
  TreeNodeState state;
  size_t n_children;
  QString field_name;
  std::optional<SpannedVal> val;
};

// turns a graph into a tree/forest
class Arborist {
public:
  Arborist() = default;

  TreeIndex add_node(ParentBranch idx, QString &field_name);
  RootIndex add_root_node(size_t root_count, QString &field_name);

  TreeNode &get_node(TreeIndex idx) { return tree[idx.idx]; }

  void set_val(TreeIndex idx, SpannedVal val) {
    tree[idx.idx].val = val;
    if (val.kind() == YaboValKind::YABOARRAY ||
        val.kind() == YaboValKind::YABOBLOCK) {
      tree[idx.idx].state = TreeNodeState::LOADED_NO_CHIlDREN;
    } else {
      tree[idx.idx].state = TreeNodeState::LOADED;
    }
  }

  TreeIndex get_child(TreeIndex parent, size_t row) const {
    return interner.at(ParentBranch{parent, row});
  }

private:
  std::vector<TreeNode> tree;
  std::unordered_map<ParentBranch, TreeIndex> interner;
};

class YaboTreeModel;

// communicates with Executor and maintains the tree structure
class FileRequester : public QObject, public NodeInfoProvider {
  Q_OBJECT
public:
  FileRequester(std::filesystem::path path, FileRef file, QString parser_name,
                bool recursive_fetch = true);
  FileRequester(QString error_msg) : error_msg(error_msg) {}
  ~FileRequester() {
    executor_thread.quit();
    executor_thread.wait();
  }
  bool has_children(TreeIndex idx) const {
    auto val = arborist->get_node(idx).val;
    if (!val.has_value()) {
      return false;
    }
    return val->kind() == YaboValKind::YABOARRAY ||
           val->kind() == YaboValKind::YABOBLOCK;
  }
  QVariant data(TreeIndex idx) const;
  FileSpan span(TreeIndex idx) const;
  TreeIndex index(TreeIndex parent, int row) const {
    return arborist->get_child(parent, row);
  }
  TreeIndex parent_index(TreeIndex idx) const {
    return arborist->get_node(idx).idx.parent;
  }
  int parent_row(TreeIndex idx) const {
    return arborist->get_node(idx).idx.row;
  }
  int child_rows(TreeIndex idx) const {
    return arborist->get_node(idx).n_children;
  }

  QString field_name(TreeIndex idx) const {
    return arborist->get_node(idx).field_name;
  }
  bool can_fetch_children(TreeIndex idx);
  void fetch_children(TreeIndex idx, RootIndex root);
  QColor color(TreeIndex idx) const;

  QString error_message() const { return error_msg; }
  FileRef file_ref() const noexcept { return file; }
  const uint8_t *file_base_addr() const noexcept { return file->span().data(); }

  YaboTreeModel &get_tree_model() { return *tree_model; }
  void set_parser(QString name);
  void set_bubble(TreeIndex idx);
  QString node_name(Node idx) const override;
  QColor node_color(Node idx) const override;

public slots:
  void process_response(Response resp);
  void change_root(Node node);

signals:
  void request(Request req);
  void parse_request(Meta meta, QString func_name);
  void update_graph(GraphUpdate update);
  void root_changed(Node node);
  void new_node(NodeRange node);

private:
  void create_tree_model(QString parser_name);
  void set_value(TreeIndex idx, SpannedVal val, RootIndex root);
  RootIndex root_idx(Node node);

  QThread executor_thread;
  std::unique_ptr<Arborist> arborist;
  std::unordered_map<QString, RootIndex> parser_root;
  std::unordered_map<YaboVal, RootIndex> nominal_bubbles;
  size_t root_count = 0;
  GraphUpdate graph_update;
  std::unique_ptr<YaboTreeModel> tree_model;
  FileRef file;
  using RootCause = std::variant<QString, YaboVal>;
  std::vector<RootCause> root_causes;
  bool recursive_fetch;
  // for qml to handle errors
  QString error_msg;
};

// class for qml to create FileRequester
class FileRequesterFactory : public QObject {
  Q_OBJECT
public:
  FileRequesterFactory() = default;
  std::unique_ptr<FileRequester> create_file_requester(QString parser_lib_path,
                                                       QString file_path,
                                                       QString parser_name,
                                                       bool recursive_fetch);
};
