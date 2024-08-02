#pragma once
#include "node.hpp"
#include "request.hpp"

class SelectionState : public QObject {
  Q_OBJECT
public:
  SelectionState() = default;
  std::optional<TreeIndex> get_selection() const noexcept {
    return selected_idx;
  }
  std::optional<Node> get_root() const noexcept { return selected_root; }
  void set_selection(TreeIndex idx);
  void clear_selection();
  void set_root(Node root);

  void undo();
  void redo();

  void jump_addr(size_t addr);

signals:
  void begin_root_change();
  void root_changed(Node node);
  void selection_changed(std::optional<TreeIndex> idx);
  void goto_addr(size_t addr);

private:
  void set_root_internal(Node root);
  std::optional<TreeIndex> selected_idx = {};
  std::optional<Node> selected_root = {};
  std::vector<Node> undo_stack = {};
  size_t undo_stack_idx = 0;
};
