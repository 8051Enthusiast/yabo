#include "graphview.hpp"
#include "graph.hpp"

GraphView::GraphView(QWidget *parent) : QGraphicsView(parent) {}

void GraphView::setScene(GraphScene *scene) {
  QGraphicsView::setScene(scene);
  follow_node = true;
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
