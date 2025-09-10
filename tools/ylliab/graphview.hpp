#pragma once

#include "node.hpp"
#include <QGraphicsView>

class GraphScene;
class GraphNodeItem;

class GraphView : public QGraphicsView {
  Q_OBJECT
public:
  GraphView(QWidget *parent = nullptr);
  void setScene(GraphScene *scene);

private slots:
  void on_selected_moved(GraphNodeItem *node, bool node_changed);
  void scrollContentsBy(int dx, int dy) override;
  void mouseMoveEvent(QMouseEvent *event) override;
  void mousePressEvent(QMouseEvent *event) override;
  void mouseReleaseEvent(QMouseEvent *event) override;
  void wheelEvent(QWheelEvent *event) override;

private:
  GraphScene *graph_scene() const;
  GraphNodeItem *node_at(QPoint pos) const;
  bool follow_node = false;
  std::optional<Node> dragged_node = {};
  bool pin_dragged_node;
  QPointF last_pos;
};