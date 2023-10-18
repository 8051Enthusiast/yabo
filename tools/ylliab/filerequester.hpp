#pragma once
#include <filesystem>
#include <format>
#include <memory>
#include <system_error>

#include <QObject>
#include <qqmlintegration.h>
#include <qthread.h>
#include <qtmetamacros.h>
#include <qvariant.h>

#include <dlfcn.h>

#include "request.hpp"
#include "yabo.hpp"
#include "yabotreemodel.hpp"

struct ExecutorError : public std::exception {
  ExecutorError(std::string msg) : message(msg) {}
  const char *what() { return message.c_str(); }
  std::string message;
};

class Executor : public QObject {
  Q_OBJECT
public:
  Executor(std::filesystem::path path, std::vector<uint8_t> &&file);
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
  void execute_parser_slot(Meta meta, std::string func_name) {
    auto resp = execute_parser(meta, func_name.c_str());
    if (resp.has_value()) {
      emit response(std::move(resp.value()));
    } else {
      emit response(Response(meta));
    }
  }
signals:
  void response(Response resp);

private:
  YaboValCache vals;
  void *lib;
  std::filesystem::path tmp_file;
  std::vector<uint8_t> file;
};

struct ParentBranch {
  bool operator==(const ParentBranch &other) const noexcept {
    return parent == other.parent && row == other.row;
  }

  TreeIndex parent;
  int row;
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
  std::string field_name;
  std::optional<YaboVal> val;
};

// turns a graph into a tree/forest
class Arborist {
public:
  Arborist() = default;

  TreeIndex add_node(TreeNode node) {
    tree.push_back(node);
    auto idx = TreeIndex{tree.size() - 1};
    interner.insert({node.idx, idx});
    return idx;
  }

  TreeNode &get_node(TreeIndex idx) { return tree[idx.idx]; }

  void set_val(TreeIndex idx, YaboVal val) {
    tree[idx.idx].val = val;
    if (val.kind() == YaboValKind::YABOARRAY ||
        val.kind() == YaboValKind::YABOBLOCK) {
      tree[idx.idx].state = TreeNodeState::LOADED_NO_CHIlDREN;
    } else {
      tree[idx.idx].state = TreeNodeState::LOADED;
    }
  }

  TreeIndex get_child(TreeIndex parent, int row) const {
    return interner.at(ParentBranch{parent, row});
  }

private:
  std::vector<TreeNode> tree;
  std::unordered_map<ParentBranch, TreeIndex> interner;
};

class YaboTreeModel;

// communicates with Executor and maintains the tree structure
class FileRequester : public QObject {
  Q_OBJECT
  QML_ELEMENT
public:
  FileRequester(std::filesystem::path path, std::vector<uint8_t> &&file);
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
  QVariant data(TreeIndex idx);
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

  std::string &field_name(TreeIndex idx) const {
    return arborist->get_node(idx).field_name;
  }
  bool can_fetch_children(TreeIndex idx);
  void fetch_children(TreeIndex idx, YaboTreeModel *tree_model);

  Q_INVOKABLE YaboTreeModel *create_tree_model(QString parser_name);

  Q_INVOKABLE QString error_message() const { return error_msg; }

public slots:
  void process_response(Response resp);

signals:
  void request(Request req);
  void parse_request(Meta meta, std::string func_name);

private:
  QThread executor_thread;
  std::unique_ptr<Arborist> arborist;
  std::unordered_map<std::string, TreeIndex> parser_root;
  // for qml to handle errors
  QString error_msg;
};

// class for qml to create FileRequester
class FileRequesterFactory : public QObject {
  Q_OBJECT
  QML_ELEMENT
  QML_SINGLETON
public:
  FileRequesterFactory() = default;
  Q_INVOKABLE FileRequester *create_file_requester(QString parser_lib_path,
                                                   QString file_path);
};
