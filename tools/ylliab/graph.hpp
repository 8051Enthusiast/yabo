#pragma once
#include "node.hpp"

#include <complex>
#include <qgraphicsscene.h>
#include <unordered_set>

#include <QGraphicsItem>
#include <QGraphicsScene>
#include <QObject>
#include <QTimer>
#include <qcolor.h>
#include <qwindowdefs.h>

using complex = std::complex<float>;

struct Edge {
  bool operator==(const Edge &other) const noexcept {
    return (start.idx == other.start.idx && end.idx == other.end.idx) ||
           (start.idx == other.end.idx && end.idx == other.start.idx);
  }
  Node start;
  Node end;
};

template <> struct std::hash<Edge> {
  std::size_t operator()(const Edge &k) const noexcept {
    auto start = k.start.idx * (size_t)11400714819323198485u;
    auto end = k.end.idx * (size_t)11400714819323198485u;
    return 17 * (start ^ end) + (start + end) + 9 * start * end;
  }
};

struct EdgeWithNewNode {
  Node start;
  Node end;
};

typedef std::variant<Node, Edge, EdgeWithNewNode> GraphComponent;

struct GraphUpdate {
  GraphUpdate() : new_components() {}
  std::vector<GraphComponent> new_components;
};

Q_DECLARE_METATYPE(GraphUpdate)

struct PositionsUpdate {
  std::vector<float> x;
  std::vector<float> y;
  std::vector<Edge> new_edges;
};

Q_DECLARE_METATYPE(PositionsUpdate)

class Graph : public QObject {
  Q_OBJECT
public:
  Graph(Node root, QObject *parent = nullptr);
public slots:
  void update_graph(GraphUpdate update);
private slots:
  void step();
signals:
  void positions_update(PositionsUpdate update);

private:
  void repeal_all();
  void attract_edges();
  void apply_force();
  void update_center();
  void add_graph_component(GraphComponent component);
  void add_edge(Edge);
  void add_node(Node, complex pos);
  void add_node(Node node) { add_node(node, 0); }
  void add_edge_with_new_node(EdgeWithNewNode);
  complex node_pos(Node idx) const { return {x[idx.idx], y[idx.idx]}; }
  complex node_force(Node idx) const { return {fx[idx.idx], fy[idx.idx]}; }
  void set_node_force(Node idx, complex force) {
    fx[idx.idx] = force.real();
    fy[idx.idx] = force.imag();
  }
  std::vector<unsigned int> outdegree;
  std::unordered_set<Edge> edges;
  std::vector<Edge> new_edges;
  std::vector<float> x;
  std::vector<float> y;
  std::vector<float> fx;
  std::vector<float> fy;
  complex center;
  QTimer *timer;
  static constexpr float scale_factor = 100.0;
  static constexpr float repulse_const = 100.0;
  static constexpr float max_force = 10.0;
  static constexpr float spring_const = 0.1;
};

class GraphScene;

class GraphNodeItem : public QGraphicsSimpleTextItem {
public:
  GraphNodeItem(QGraphicsItem *parent, NodeInfoProvider &provider,
                GraphScene &scene, Node idx);
  void paint(QPainter *painter, const QStyleOptionGraphicsItem *option,
             QWidget *widget = nullptr) override;
  QRectF boundingRect() const override {
    return QGraphicsSimpleTextItem::boundingRect().adjusted(-5, -2, 5, 2);
  }
  void setCenterPos(QPointF pos) { setPos(pos - boundingRect().center()); }
  void setCenterPos(float x, float y) { setCenterPos({x, y}); }
  void setSelected(bool selected) {
    this->selected = selected;
    update();
  }

protected:
  void mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event) override;

private:
  Node idx;
  GraphScene &scene;
  QColor color;
  bool selected = false;
};

class GraphScene : public QGraphicsScene {
  Q_OBJECT
public:
  GraphScene(QObject *parent, NodeInfoProvider &info_provider, Graph &graph)
      : QGraphicsScene(parent), info_provider(info_provider),
        selected(Node{0}) {
    QObject::connect(&graph, &Graph::positions_update, this,
                     &GraphScene::update_positions);
  }
  void node_double_clicked(Node node) { info_provider.change_root(node); }
public slots:
  void update_positions(PositionsUpdate update);
  void select_node(Node idx);

private:
  NodeInfoProvider &info_provider;
  std::vector<GraphNodeItem *> nodes;
  std::vector<std::pair<QGraphicsLineItem *, Edge>> edges;
  Node selected;
};
