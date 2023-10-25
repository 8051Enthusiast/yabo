#include "yabotreemodel.hpp"
#include "filerequester.hpp"
#include "request.hpp"
#include <qcolor.h>

enum Column {
  FIELD = 0,
  VALUE = 1,
  ADDR = 2,
  DEBUG = 3,
};

YaboTreeModel::YaboTreeModel(FileRequester &file_requester,
                             std::string &&parser_name, TreeIndex root_id)
    : QAbstractItemModel(&file_requester), file_requester(file_requester),
      parser_name(parser_name), root_id(root_id), undo_stack(1, root_id) {}
QModelIndex YaboTreeModel::index(int row, int column,
                                 const QModelIndex &parent) const {
  TreeIndex new_index;
  if (!parent.isValid()) {
    new_index = INVALID_PARENT;
  } else if (parent.internalId() == INVALID_PARENT.idx) {
    assert(parent.row() == 0);
    new_index = root_id;
  } else {
    new_index = to_tree_index(parent);
  }
  auto ret = createIndex(row, column, new_index.idx);
  return ret;
}

QModelIndex YaboTreeModel::parent(const QModelIndex &index) const {
  auto parent = TreeIndex{index.internalId()};
  if (parent == INVALID_PARENT) {
    return QModelIndex();
  }
  if (parent == root_id) {
    return createIndex(0, 0, INVALID_PARENT.idx);
  }
  auto parent_parent = file_requester.parent_index(parent);
  auto parent_row = file_requester.parent_row(parent);
  auto ret = createIndex(parent_row, 0, parent_parent.idx);
  return ret;
}

int YaboTreeModel::rowCount(const QModelIndex &parent) const {
  if (!parent.isValid())
    return 1;

  auto count = file_requester.child_rows(to_tree_index(parent));
  return count;
}

int YaboTreeModel::columnCount(const QModelIndex &parent) const {
  return NUM_COLUMNS;
}

QVariant YaboTreeModel::color(const QModelIndex &index) const {
  if (index.column() != Column::VALUE) {
    return QVariant();
  }
  return file_requester.color(to_tree_index(index));
}

QVariant YaboTreeModel::data(const QModelIndex &index, int role) const {
  if (role != Qt::DisplayRole) {
    if (role == Qt::BackgroundRole) {
      return color(index);
    }
    return QVariant();
  }
  if (!index.isValid())
    return QVariant();

  switch (index.column()) {
  case Column::VALUE:
    return file_requester.data(to_tree_index(index));
  case Column::FIELD:
    return QString::fromStdString(
        file_requester.field_name(to_tree_index(index)));
  case Column::ADDR: {
    auto span = file_requester.span(to_tree_index(index));
    if (!span.data()) {
      return QVariant();
    }
    auto relative_addr_start = span.data() - file_requester.file_base_addr();
    auto addr_str =
        QString::asprintf("0x%08zx [%zd]", relative_addr_start, span.size());
    return addr_str;
  }
  case Column::DEBUG: {
    auto model_index_str = QString::number(index.internalId(), 16) + " " +
                           QString::number(index.row()) + " " +
                           QString::number(index.column()) + " | " +
                           QString::number(to_tree_index(index).idx, 16);
    return model_index_str;
  }
  default:
    return QVariant();
  }
  return QVariant();
}

QVariant YaboTreeModel::headerData(int section, Qt::Orientation orientation,
                                   int role) const {
  if (orientation != Qt::Orientation::Horizontal || role != Qt::DisplayRole) {
    return QVariant();
  }
  switch (section) {
  case Column::FIELD:
    return "Field Name";
  case Column::VALUE:
    return "Data";
  case Column::ADDR:
    return "Address";
  case Column::DEBUG:
    return "Debug";
  default:
    return QVariant();
  }
}

bool YaboTreeModel::hasChildren(const QModelIndex &parent) const {
  if (!parent.isValid())
    return true;

  auto ret = file_requester.has_children(to_tree_index(parent));
  return ret;
}

bool YaboTreeModel::canFetchMore(const QModelIndex &parent) const {
  if (!parent.isValid())
    return false;

  auto ret = file_requester.can_fetch_children(to_tree_index(parent));
  return ret;
}

void YaboTreeModel::fetchMore(const QModelIndex &parent) {
  if (!parent.isValid())
    return;

  file_requester.fetch_children(to_tree_index(parent), root_id);
}

QModelIndex YaboTreeModel::to_qindex(TreeIndex idx, int column) const {
  if (idx == INVALID_PARENT) {
    return QModelIndex();
  }
  auto parent = file_requester.parent_index(idx);
  if (parent == INVALID_PARENT) {
    return createIndex(0, column, INVALID_PARENT.idx);
  }
  auto row = file_requester.parent_row(idx);
  auto ret = createIndex(row, column, parent.idx);
  return ret;
}

void YaboTreeModel::data_changed(TreeIndex idx) {
  auto qidx_start = to_qindex(idx, 0);
  auto qidx_end = to_qindex(idx, NUM_COLUMNS - 1);
  emit dataChanged(qidx_start, qidx_end);
}

TreeIndex YaboTreeModel::to_tree_index(const QModelIndex &index) const {
  if (index.internalId() == INVALID_PARENT.idx) {
    return root_id;
  }

  return file_requester.index(TreeIndex{index.internalId()}, index.row());
}

void YaboTreeModel::begin_insert_rows(TreeIndex parent, int first, int last) {
  inserting_rows = true;
  auto idx = to_qindex(parent, 0);
  beginInsertRows(idx, first, last);
}

void YaboTreeModel::end_insert_rows() {
  if (inserting_rows) {
    endInsertRows();
  }
  inserting_rows = false;
}

void YaboTreeModel::set_root(TreeIndex new_root) {
  if (root_id == new_root) {
    return;
  }
  beginResetModel();
  root_id = new_root;
  undo_stack.erase(undo_stack.begin() + undo_stack_idx + 1, undo_stack.end());
  undo_stack.push_back(root_id);
  undo_stack_idx++;
  endResetModel();
}

void YaboTreeModel::redo() {
  if (undo_stack_idx + 1 >= undo_stack.size()) {
    return;
  }
  beginResetModel();
  root_id = undo_stack[++undo_stack_idx];
  endResetModel();
}

void YaboTreeModel::undo() {
  if (undo_stack_idx == 0) {
    return;
  }
  beginResetModel();
  root_id = undo_stack[--undo_stack_idx];
  endResetModel();
}

void YaboTreeModel::handle_doubleclick(const QModelIndex &index) {
  if (index.column() != Column::VALUE) {
    return;
  }
  file_requester.set_bubble(to_tree_index(index));
}
