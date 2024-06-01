#include "valtreeview.hpp"
#include "valtreemodel.hpp"

#include <QHeaderView>

ValTreeView::ValTreeView(QWidget *parent) : QTreeView(parent) {}

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
