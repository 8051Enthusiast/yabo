#include "valtreemodel.hpp"
#include "filerequester.hpp"
#include "request.hpp"
#include "selectionstate.hpp"
#include <QColor>
#include <QVariant>
#include <Qt>
#include <cinttypes>

ValTreeModel::ValTreeModel(FileRequester *file_requester,
                           std::shared_ptr<SelectionState> select)
    : QAbstractItemModel(file_requester), file_requester(file_requester),
      select(select) {
  connect(file_requester, &FileRequester::tree_data_changed, this,
          &ValTreeModel::data_changed);
  connect(file_requester, &FileRequester::tree_begin_insert_rows, this,
          &ValTreeModel::begin_insert_rows);
  connect(file_requester, &FileRequester::tree_end_insert_rows, this,
          &ValTreeModel::end_insert_rows);
  connect(select.get(), &SelectionState::begin_root_change, this,
          &ValTreeModel::beginResetModel);
  connect(select.get(), &SelectionState::root_changed, this,
          &ValTreeModel::endResetModel);
}
QModelIndex ValTreeModel::index(int row, int column,
                                const QModelIndex &parent) const {
  TreeIndex new_index;
  if (!select->get_root()) {
    return QModelIndex();
  }
  if (!parent.isValid()) {
    new_index = INVALID_PARENT;
  } else if (parent.internalId() == INVALID_PARENT.idx) {
    assert(parent.row() == 0);
    auto idx = current_root_tree_index();
    assert(idx);
    new_index = *idx;
  } else {
    new_index = to_tree_index(parent);
  }
  auto ret = createIndex(row, column, new_index.idx);
  return ret;
}

QModelIndex ValTreeModel::parent(const QModelIndex &index) const {
  auto parent = TreeIndex{index.internalId()};
  if (parent == INVALID_PARENT) {
    return QModelIndex();
  }
  if (parent == current_root_tree_index()) {
    return createIndex(0, 0, INVALID_PARENT.idx);
  }
  auto parent_parent = file_requester->parent_index(parent);
  auto parent_row = file_requester->parent_row(parent);
  auto ret = createIndex(parent_row, 0, parent_parent.idx);
  return ret;
}

int ValTreeModel::rowCount(const QModelIndex &parent) const {
  if (!select->get_root()) {
    return 0;
  }
  if (!parent.isValid())
    return 1;

  auto count = file_requester->child_rows(to_tree_index(parent));
  return count;
}

int ValTreeModel::columnCount(const QModelIndex &parent) const {
  return NUM_COLUMNS;
}

QVariant ValTreeModel::color(const QModelIndex &index) const {
  if (index.column() != Column::VALUE) {
    return QVariant();
  }
  auto col = file_requester->color(to_tree_index(index));
  if (col) {
    return *col;
  }
  return QVariant();
}

QVariant ValTreeModel::data(const QModelIndex &index, int role) const {
  if (!select->get_root()) {
    return QVariant();
  }
  if (role != Qt::DisplayRole) {
    if (role == Qt::BackgroundRole) {
      return color(index);
    } else if (role == Qt::ForegroundRole && color(index).isValid()) {
      return QColor(Qt::black);
    }
    return QVariant();
  }
  if (!index.isValid())
    return QVariant();

  switch (index.column()) {
  case Column::VALUE:
    return file_requester->data(to_tree_index(index));
  case Column::FIELD:
    return file_requester->field_name(to_tree_index(index));
  case Column::ADDR: {
    auto span = file_requester->span(to_tree_index(index));
    if (!span) {
      return QVariant();
    }
    auto addr_str = QString::asprintf("0x%08" PRIx64 " [%" PRIu64 "]",
                                      *span.addr(), span.size());
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

QVariant ValTreeModel::headerData(int section, Qt::Orientation orientation,
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

bool ValTreeModel::hasChildren(const QModelIndex &parent) const {
  if (!select->get_root()) {
    return false;
  }
  if (!parent.isValid()) {
    return true;
  }

  auto ret = file_requester->has_children(to_tree_index(parent));
  return ret;
}

bool ValTreeModel::canFetchMore(const QModelIndex &parent) const {
  if (!select->get_root()) {
    return false;
  }
  if (!parent.isValid()) {
    return false;
  }

  auto ret = file_requester->can_fetch_children(to_tree_index(parent));
  return ret;
}

void ValTreeModel::fetchMore(const QModelIndex &parent) {
  if (!parent.isValid()) {
    return;
  }
  auto node = select->get_root();
  if (!node) {
    return;
  }

  file_requester->fetch_children(to_tree_index(parent),
                                 file_requester->root_idx(*node));
}

QModelIndex ValTreeModel::to_qindex(TreeIndex idx, int column) const {
  if (idx == INVALID_PARENT) {
    return QModelIndex();
  }
  auto parent = file_requester->parent_index(idx);
  if (parent == INVALID_PARENT) {
    return createIndex(0, column, INVALID_PARENT.idx);
  }
  auto row = file_requester->parent_row(idx);
  auto ret = createIndex(row, column, parent.idx);
  return ret;
}

void ValTreeModel::data_changed(TreeIndex idx, RootIndex root) {
  if (Node(root) != select->get_root()) {
    return;
  }
  auto qidx_start = to_qindex(idx, 0);
  auto qidx_end = to_qindex(idx, NUM_COLUMNS - 1);
  emit dataChanged(qidx_start, qidx_end);
}

TreeIndex ValTreeModel::to_tree_index(const QModelIndex &index) const {
  if (index.internalId() == INVALID_PARENT.idx) {
    auto idx = current_root_tree_index();
    assert(idx);
    return *idx;
  }

  return file_requester->index(TreeIndex{index.internalId()}, index.row());
}

void ValTreeModel::begin_insert_rows(TreeIndex parent, int first, int last,
                                     RootIndex root) {
  if (Node(root) != select->get_root()) {
    return;
  }
  inserting_rows = true;
  auto idx = to_qindex(parent, 0);
  beginInsertRows(idx, first, last);
}

void ValTreeModel::end_insert_rows(TreeIndex parent, RootIndex root) {
  if (Node(root) != select->get_root()) {
    return;
  }
  if (inserting_rows) {
    endInsertRows();
  }
  inserting_rows = false;
}

void ValTreeModel::change_selected(const QModelIndex &current,
                                   const QModelIndex &previous) {
  if (!current.isValid()) {
    select->clear_selection();
  }
  auto idx = to_tree_index(current);
  select->set_selection(idx);
}

void ValTreeModel::handle_doubleclick(const QModelIndex &index) {
  if (index.column() == Column::VALUE) {
    auto node = file_requester->link(to_tree_index(index));
    if (node) {
      emit select->set_root(*node);
    }
  }
  if (index.column() == Column::ADDR) {
    auto span = file_requester->span(to_tree_index(index));
    if (span) {
      emit select->goto_addr(*span.addr());
    }
  }
}

std::optional<TreeIndex> ValTreeModel::current_root_tree_index() const {
  auto node = select->get_root();
  if (!node) {
    return {};
  }
  return file_requester->root_idx(*node).tree_index;
}

ByteSpan ValTreeModel::idx_span(const QModelIndex &index) const {
  auto span = file_requester->span(to_tree_index(index));
  if (!file_requester->file_ref()->is_valid_span(span)) {
    return ByteSpan();
  }
  auto byte_span = file_requester->file_ref()->byte_span(span);
  return byte_span;
}
