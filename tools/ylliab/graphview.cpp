#include "graphview.hpp"
#include "graph.hpp"
#include "node.hpp"
#include <QMouseEvent>
#include <qnamespace.h>

GraphView::GraphView(QWidget *parent) : QGraphicsView(parent) {}

void GraphView::setScene(GraphScene *scene) {
  if (this->scene) {
    disconnect(this->scene, &GraphScene::selected_node_moved, this,
               &GraphView::on_selected_moved);
  }
  QGraphicsView::setScene(scene);
  follow_node = true;
  this->scene = scene;
  connect(scene, &GraphScene::selected_node_moved, this,
          &GraphView::on_selected_moved);
}

void GraphView::on_selected_moved(GraphNodeItem *node, bool node_changed) {
  follow_node |= node_changed;
  if (follow_node) {
    if (node_changed) {
      centerOn(node);
    } else {
      ensureVisible(node);
    }
    // ensureVisible may scroll the view, which unsets follow_node
    // in scrollContentsBy
    follow_node = true;
  }
}

void GraphView::scrollContentsBy(int dx, int dy) {
  follow_node = false;
  QGraphicsView::scrollContentsBy(dx, dy);
}

GraphNodeItem *GraphView::node_at(QPoint pos) const {
  auto item = itemAt(pos);
  if (!item) {
    return nullptr;
  }
  return qgraphicsitem_cast<GraphNodeItem *>(item);
}

void GraphView::mousePressEvent(QMouseEvent *event) {
  if (event->button() & (Qt::LeftButton | Qt::RightButton)) {
    pin_dragged_node = true;
  }
  if (event->button() != Qt::RightButton) {
    return QGraphicsView::mousePressEvent(event);
  }
  auto node = node_at(event->pos());
  if (node) {
    follow_node = false;
    last_pos = mapToScene(event->pos());
    dragged_node = node->node();
    auto pos = node->centerPos();
    scene->move_node(pos, *dragged_node, true);
  }
  QGraphicsView::mousePressEvent(event);
}

void GraphView::mouseMoveEvent(QMouseEvent *event) {
  if (!(event->buttons() & Qt::RightButton)) {
    return QGraphicsView::mouseMoveEvent(event);
  }
  if (dragged_node) {
    auto node = scene->node(*dragged_node);
    auto pos = mapToScene(event->pos());
    auto diff = pos - last_pos;
    auto new_pos = node->centerPos() + diff;
    scene->move_node(new_pos, *dragged_node, true);
    last_pos = pos;
    pin_dragged_node = false;
  }
  QGraphicsView::mouseMoveEvent(event);
}

void GraphView::mouseReleaseEvent(QMouseEvent *event) {
  if (event->button() == Qt::RightButton && dragged_node) {
    auto node = scene->node(*dragged_node);
    auto pos = node->centerPos();
    scene->move_node(pos, *dragged_node, pin_dragged_node);
    dragged_node = {};
  }
  QGraphicsView::mouseReleaseEvent(event);
}
