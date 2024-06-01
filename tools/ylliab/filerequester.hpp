#pragma once
#include <filesystem>
#include <format>
#include <memory>
#include <system_error>

#include <QObject>
#include <qthread.h>
#include <qvariant.h>

#include <unordered_set>

#include "filecontent.hpp"
#include "graph.hpp"
#include "request.hpp"
#include "yabo.hpp"

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
  LOADED_INCOMPLETE_CHIlDREN,
  LOADING_CHILDREN,
  LOADED,
  ERROR,
};

struct TreeNode {
  ParentBranch idx;
  TreeNodeState state;
  size_t n_children;
  QString field_name;
  std::optional<SpannedHandle> val;
  std::optional<ValHandle> continuation;
};

// turns a graph into a tree/forest
class Arborist {
public:
  Arborist() = default;

  TreeIndex add_node(ParentBranch idx, QString &field_name);
  RootIndex add_root_node(size_t root_count, QString &field_name);

  TreeNode &get_node(TreeIndex idx) { return tree[idx.idx]; }

  void set_val(TreeIndex idx, SpannedHandle val) {
    tree[idx.idx].val = val;
    if (val.kind == YaboValKind::YABOARRAY ||
        val.kind == YaboValKind::YABOBLOCK) {
      tree[idx.idx].state = TreeNodeState::LOADED_INCOMPLETE_CHIlDREN;
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

class ValTreeModel;

// communicates with Executor and maintains the tree structure
class FileRequester : public QObject,
                      public NodeInfoProvider,
                      public ParseRequester {
  Q_OBJECT
public:
  FileRequester(std::filesystem::path path, FileRef file,
                bool recursive_fetch = true);
  FileRequester(QString error_msg) : error_msg(error_msg) {}
  ~FileRequester() {
    if (executor_thread) {
      executor_thread->quit();
    }
  }
  bool has_children(TreeIndex idx) const {
    auto val = arborist->get_node(idx).val;
    if (!val.has_value()) {
      return false;
    }
    return val->kind == YaboValKind::YABOARRAY ||
           val->kind == YaboValKind::YABOBLOCK;
  }
  QVariant data(TreeIndex idx) const;
  std::optional<Node> link(TreeIndex idx) const;
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
  std::optional<QColor> color(TreeIndex idx) const;

  QString error_message() const { return error_msg; }
  FileRef file_ref() const noexcept { return file; }
  const uint8_t *file_base_addr() const noexcept { return file->span().data(); }

  RootIndex set_parser(QString name, size_t pos = 0);
  QString node_name(Node idx) const override;
  QColor node_color(Node idx) const override;
  std::optional<std::pair<size_t, size_t>> node_range(Node idx) const override;
  std::optional<std::pair<size_t, size_t>>
  idx_range(TreeIndex idx) const override;

  RootIndex run_parse(QString func_name, size_t pos) override {
    return set_parser(func_name, pos);
  }

  void start_executor_thread() { executor_thread->start(); }
  RootIndex root_idx(Node node) const override;

public slots:
  void process_response(Response resp);

signals:
  void request(Request req);
  void parse_request(Meta meta, QString func_name, size_t pos);
  void update_graph(GraphUpdate update);
  void new_node(NodeRange node);
  void tree_data_changed(TreeIndex idx, RootIndex root);
  void tree_begin_insert_rows(TreeIndex parent, int first, int last,
                              RootIndex root);
  void tree_end_insert_rows(TreeIndex parent, RootIndex root);

private:
  QColor generate_new_node_color(ValHandle val) const;
  QColor generate_new_node_color(QString val, size_t pos) const;
  void set_value(TreeIndex idx, SpannedHandle val, RootIndex root);

  QThread *executor_thread = nullptr;
  std::unique_ptr<Arborist> arborist;
  std::map<std::pair<QString, size_t>, RootIndex> parser_root;
  std::unordered_map<ValHandle, RootIndex> nominal_bubbles;
  size_t root_count = 0;
  GraphUpdate graph_update;
  FileRef file;
  struct RootInfo {
    std::optional<ValHandle> cause;
    QString name;
    QColor color;
    bool visited;
  };
  std::vector<RootInfo> root_info;
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
                                                       bool recursive_fetch);
  std::unique_ptr<FileRequester> create_file_requester(QString parser_lib_path,
                                                       FileRef file,
                                                       bool recursive_fetch);
};
