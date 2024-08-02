#include "valtreeview.hpp"
#include "valtreemodel.hpp"

#include <QAction>
#include <QFileDialog>
#include <QHeaderView>
#include <QMenu>

ValTreeView::ValTreeView(QWidget *parent) : QTreeView(parent) {
  setContextMenuPolicy(Qt::CustomContextMenu);
  connect(this, &ValTreeView::customContextMenuRequested, this,
          &ValTreeView::context_menu);
}

void ValTreeView::reset() {
  QTreeView::reset();
  auto root = model()->index(0, 0);
  if (root.isValid() && model()->hasChildren(root)) {
    expand(root);
  }
}

void ValTreeView::setModel(ValTreeModel *new_model) {
  if (auto old_model = dynamic_cast<ValTreeModel *>(this->model())) {
    disconnect(selectionModel(), &QItemSelectionModel::currentChanged,
               old_model, &ValTreeModel::change_selected);
  }
  QTreeView::setModel(new_model);
  connect(selectionModel(), &QItemSelectionModel::currentChanged, new_model,
          &ValTreeModel::change_selected);
  auto root = new_model->index(0, 0);
  if (root.isValid() && new_model->hasChildren(root)) {
    expand(root);
  }
  auto header_view = header();
  header_view->setSectionResizeMode(0, QHeaderView::ResizeToContents);
  header_view->setStretchLastSection(false);
  header_view->setSectionResizeMode(ValTreeModel::NUM_COLUMNS - 1,
                                    QHeaderView::Stretch);
  header_view->setMinimumSectionSize(150);
}

void ValTreeView::rowsInserted(const QModelIndex &parent, int start, int end) {
  QTreeView::rowsInserted(parent, start, end);
  auto root = model()->index(0, 0);
  if (start == 0 && root.isValid() && root == parent) {
    expand(root);
  }
}

void ValTreeView::dataChanged(const QModelIndex &topLeft,
                              const QModelIndex &bottomRight,
                              const QVector<int> &roles) {
  QTreeView::dataChanged(topLeft, bottomRight, roles);
  auto root = model()->index(0, 0);
  if (root.isValid() && root == topLeft) {
    expand(root);
  }
}

void ValTreeView::context_menu(const QPoint &pos) {
  auto index = indexAt(pos);
  if (!index.isValid()) {
    return;
  }
  auto model = dynamic_cast<ValTreeModel *>(this->model());
  if (!model) {
    return;
  }
  auto span = model->idx_span(index);
  if (!span.data()) {
    return;
  }
  auto field_column = model->index(index.row(), Column::FIELD, index.parent());
  auto name = model->data(field_column, Qt::DisplayRole).toString();
  auto menu = new QMenu(this);
  auto action = new QAction("Save Bytes to file", menu);
  connect(action, &QAction::triggered, [span, name]() {
    QByteArray data(reinterpret_cast<const char *>(span.data()), span.size());
    QFileDialog::saveFileContent(data, name + ".bin");
  });
  menu->addAction(action);
  menu->popup(mapToGlobal(pos));
}
