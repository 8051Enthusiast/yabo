#pragma once

#include <QTreeView>
#include <qwidget.h>

class ValTreeModel;

class ValTreeView : public QTreeView {
  Q_OBJECT
public:
  ValTreeView(QWidget *parent = nullptr);
  void reset() override;
  void setModel(ValTreeModel *model);

protected:
  void rowsInserted(const QModelIndex &parent, int start, int end) override;
  void dataChanged(const QModelIndex &topLeft, const QModelIndex &bottomRight,
                   const QVector<int> &roles = QVector<int>()) override;
};