#pragma once

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

private:
  bool follow_node;
};