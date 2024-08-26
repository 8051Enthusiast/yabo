#pragma once
#include "request.hpp"
#include <QAbstractItemModel>
#include <memory>
#include <qabstractitemmodel.h>

class FileRequester;
class SelectionState;

enum Column {
  FIELD = 0,
  VALUE = 1,
  ADDR = 2,
  DEBUG = 3,
};


class ValTreeModel : public QAbstractItemModel {
  Q_OBJECT
public:
  ValTreeModel(FileRequester *file_requester,
               std::shared_ptr<SelectionState> select);

  // AbstractItemModel interface
  QModelIndex index(int row, int column,
                    const QModelIndex &parent = QModelIndex()) const override;
  QModelIndex parent(const QModelIndex &index) const override;

  int rowCount(const QModelIndex &parent = QModelIndex()) const override;
  int columnCount(const QModelIndex &parent = QModelIndex()) const override;

  QVariant data(const QModelIndex &index,
                int role = Qt::DisplayRole) const override;
  QVariant headerData(int section, Qt::Orientation orientation,
                      int role = Qt::DisplayRole) const override;

  bool hasChildren(const QModelIndex &parent = QModelIndex()) const override;
  bool canFetchMore(const QModelIndex &parent) const override;
  void fetchMore(const QModelIndex &parent) override;

  void handle_doubleclick(const QModelIndex &index);

  ByteSpan idx_span(const QModelIndex &index) const;
  void change_selected(const QModelIndex &current, const QModelIndex &previous);
  TreeIndex to_tree_index(const QModelIndex &index) const;
  QModelIndex to_qindex(TreeIndex idx, int column) const;

  constexpr static int NUM_COLUMNS = 3;

private slots:
  void data_changed(TreeIndex idx, RootIndex root);
  void begin_insert_rows(TreeIndex parent, int first, int last, RootIndex root);
  void end_insert_rows(TreeIndex parent, RootIndex root);

private:
  QVariant color(const QModelIndex &index) const;
  std::optional<TreeIndex> current_root_tree_index() const;

  FileRequester *file_requester;
  std::shared_ptr<SelectionState> select;
  bool inserting_rows = false;
};
