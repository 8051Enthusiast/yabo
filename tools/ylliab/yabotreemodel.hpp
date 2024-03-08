#pragma once
#include "node.hpp"
#include "request.hpp"
#include <QAbstractItemModel>
#include <qabstractitemmodel.h>

class FileRequester;

class YaboTreeModel : public QAbstractItemModel {
  Q_OBJECT
public:
  YaboTreeModel(FileRequester *file_requester);

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

  void set_root(RootIndex new_root);

  void handle_doubleclick(const QModelIndex &index);

  void undo();
  void redo();

  void change_selected(const QModelIndex &current, const QModelIndex &previous);

signals:
  void expand(const QModelIndex &index);

private slots:
  void data_changed(TreeIndex idx, RootIndex root);
  void begin_insert_rows(TreeIndex parent, int first, int last, RootIndex root);
  void end_insert_rows(TreeIndex parent, RootIndex root);
  void change_root(Node idx);

private:
  TreeIndex to_tree_index(const QModelIndex &index) const;
  QModelIndex to_qindex(TreeIndex idx, int column) const;
  QVariant color(const QModelIndex &index) const;

  FileRequester *file_requester;
  RootIndex root_id;
  bool inserting_rows = false;
  std::vector<RootIndex> undo_stack;
  size_t undo_stack_idx = 0;
  constexpr static int NUM_COLUMNS = 3;
};
