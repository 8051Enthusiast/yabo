#include "yabotreemodel.hpp"
#include "filerequester.hpp"
#include "request.hpp"

enum Column {
  FIELD = 0,
  VALUE = 1,
  DEBUG = 2,
};

YaboTreeModel::YaboTreeModel(FileRequester &file_requester,
                             std::string &&parser_name, TreeIndex root_id)
    : QAbstractItemModel(&file_requester), file_requester(file_requester),
      parser_name(parser_name), root_id(root_id) {}
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
  qDebug() << "index" << row << column << parent << " -> " << ret;
  return ret;
}

QModelIndex YaboTreeModel::parent(const QModelIndex &index) const {
  auto parent = TreeIndex{index.internalId()};
  if (parent == INVALID_PARENT) {
    return QModelIndex();
  }
  auto parent_parent = file_requester.parent_index(parent);
  auto parent_row = file_requester.parent_row(parent);
  auto ret = createIndex(parent_row, 0, parent_parent.idx);
  qDebug() << "parent" << index << " -> " << ret;
  return ret;
}

int YaboTreeModel::rowCount(const QModelIndex &parent) const {
  if (!parent.isValid())
    return 1;

  auto count = file_requester.child_rows(to_tree_index(parent));
  qDebug() << "row count" << parent << " -> " << count;
  return count;
}

int YaboTreeModel::columnCount(const QModelIndex &parent) const {
  return NUM_COLUMNS;
}

QVariant YaboTreeModel::data(const QModelIndex &index, int role) const {
  if (!index.isValid())
    return QString();

  if (index.column() == Column::VALUE && role == Qt::DisplayRole) {
    return file_requester.data(to_tree_index(index));
  }
  if (index.column() == Column::FIELD && role == Qt::DisplayRole) {
    return QString::fromStdString(
        file_requester.field_name(to_tree_index(index)));
  }
  if (index.column() == Column::DEBUG && role == Qt::DisplayRole) {
    auto model_index_str = QString::number(index.internalId(), 16) + " " +
                           QString::number(index.row()) + " " +
                           QString::number(index.column()) + " | " +
                           QString::number(to_tree_index(index).idx, 16);
    return model_index_str;
  }
  return QString();
}

bool YaboTreeModel::hasChildren(const QModelIndex &parent) const {
  if (!parent.isValid())
    return true;


  auto ret = file_requester.has_children(to_tree_index(parent));
  qDebug() << "has children" << parent << " -> " << ret;
  return ret;
}

bool YaboTreeModel::canFetchMore(const QModelIndex &parent) const {
  if (!parent.isValid())
    return false;

  auto ret = file_requester.can_fetch_children(to_tree_index(parent));
  qDebug() << "can fetch more" << parent << " -> " << ret;
  return ret;
}

void YaboTreeModel::fetchMore(const QModelIndex &parent) {
  if (!parent.isValid())
    return;

  file_requester.fetch_children(to_tree_index(parent), this);
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
  qDebug() << "to qindex" << idx.idx << column << " -> " << ret;
  return ret;
}

void YaboTreeModel::data_changed(TreeIndex idx) {
  auto qidx_start = to_qindex(idx, 0);
  auto qidx_end = to_qindex(idx, NUM_COLUMNS - 1);
  qDebug() << "data changed" << qidx_start << qidx_end;
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
  qDebug() << "begin insert rows" << idx << first << last;
  beginInsertRows(idx, first, last);
}

void YaboTreeModel::end_insert_rows() {
  if (inserting_rows) {
    qDebug() << "end insert rows";
    endInsertRows();
  }
  inserting_rows = false;
}