#include "selectionstate.hpp"

void SelectionState::set_selection(TreeIndex idx) {
  if (idx == selected_idx) {
    return;
  }
  selected_idx = idx;
  emit selection_changed(selected_idx);
}

void SelectionState::clear_selection() {
  if (!selected_idx) {
    return;
  }
  selected_idx = {};
  emit selection_changed({});
}

void SelectionState::set_root(Node root) {
  if (root == selected_root) {
    return;
  }
  undo_stack.erase(undo_stack.begin() + undo_stack_idx, undo_stack.end());
  undo_stack.push_back(root);
  undo_stack_idx++;
  set_root_internal(root);
}

void SelectionState::redo() {
  if (undo_stack_idx >= undo_stack.size()) {
    return;
  }
  set_root_internal(undo_stack[++undo_stack_idx - 1]);
}

void SelectionState::undo() {
  if (undo_stack_idx <= 1) {
    return;
  }
  set_root_internal(undo_stack[--undo_stack_idx - 1]);
}

void SelectionState::set_root_internal(Node root) {
  emit begin_root_change();
  selected_root = root;
  selected_idx = {};
  emit root_changed(root);
  emit selection_changed({});
}
