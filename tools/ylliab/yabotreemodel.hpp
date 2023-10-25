#pragma once
#include "request.hpp"
#include <QAbstractItemModel>
#include <qabstractitemmodel.h>

class FileRequester;

class YaboTreeModel : public QAbstractItemModel {
  Q_OBJECT
public:
  YaboTreeModel(FileRequester &file_requester, std::string &&parser_name,
                TreeIndex root_id);

  // AbstractItemModel interface
  QModelIndex index(int row, int column,
                    const QModelIndex &parent = QModelIndex()) const override;
  QModelIndex parent(const QModelIndex &index) const override;

  int rowCount(const QModelIndex &parent = QModelIndex()) const override;
  int columnCount(const QModelIndex &parent = QModelIndex()) const override;

  QVariant data(const QModelIndex &index,
                int role = Qt::DisplayRole) const override;
  QVariant headerData(int section, Qt::Orientation orientation, int role = Qt::DisplayRole) const override;

  bool hasChildren(const QModelIndex &parent = QModelIndex()) const override;
  bool canFetchMore(const QModelIndex &parent) const override;
  void fetchMore(const QModelIndex &parent) override;

  void data_changed(TreeIndex idx);
  void begin_insert_rows(TreeIndex parent, int first, int last);
  void end_insert_rows();

  TreeIndex get_root() const noexcept { return root_id; }

private:
  TreeIndex to_tree_index(const QModelIndex &index) const;
  QModelIndex to_qindex(TreeIndex idx, int column) const;

  FileRequester &file_requester;
  std::string parser_name;
  TreeIndex root_id;
  bool inserting_rows = false;
  constexpr static int NUM_COLUMNS = 3;
};
