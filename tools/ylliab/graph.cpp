#include "graph.hpp"
#include "selectionstate.hpp"

#include <QPainter>
#include <QTimer>
#include <cmath>
#include <qtransform.h>

using namespace std::complex_literals;

Graph::Graph(QObject *parent) : QObject(parent), outdegree(1, 0), center(0) {
  std::random_device rd;
  rng = std::mt19937(rd());
  timer = new QTimer(this);
  timer->setInterval(50);
  connect(timer, &QTimer::timeout, this, &Graph::step);
  timer->start();
}

void Graph::add_edge(Edge edge) {
  if (edge.start == edge.end) {
    return;
  }
  auto [_, inserted] = edges.insert(edge);
  if (!inserted) {
    return;
  }
  outdegree[edge.start.idx]++;
  new_edges.push_back(edge);
}

void Graph::add_node(Node node, complex pos) {
  // we need to add some randomness to prevent nodes from being
  // at the exact same position and not moving from each other
  // because they each repel each other infinitely strong
  std::uniform_real_distribution<float> dist(-1, 1);
  x.push_back(pos.real() + dist(rng));
  y.push_back(pos.imag() + dist(rng));
  fx.push_back(0);
  fy.push_back(0);
  outdegree.push_back(0);
  pinned.push_back(false);
}

void Graph::add_edge_with_new_node(EdgeWithNewNode edge) {
  /*
                               new_direction
                                  v
  (center)--------------->(start)-->.
           center_relative          | offset (depending on outdeg)
                                    v
                                  (end)

  */
  auto start = node_pos(edge.start);
  auto center_relative = start - center;
  complex new_direction;
  if (center_relative != 0.0f) {
    new_direction = center_relative / std::abs(center_relative) * scale_factor;
  } else {
    new_direction = scale_factor;
  }
  auto outdeg = (signed)outdegree[edge.start.idx];
  auto sign = (outdeg % 2) ? 1 : -1;
  auto offset = (outdeg + 1) / 2 * sign;
  auto end = start + new_direction * (1.0f + 0.5if * (float)offset);
  add_node(edge.end, end);
  add_edge(Edge{edge.start, edge.end});
}

void Graph::add_graph_component(GraphComponent component) {
  std::visit(
      [this](auto &&arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, Node>) {
          add_node(arg, center);
        } else if constexpr (std::is_same_v<T, Edge>) {
          add_edge(arg);
        } else if constexpr (std::is_same_v<T, EdgeWithNewNode>) {
          add_edge_with_new_node(arg);
        }
      },
      component);
}

void Graph::update_graph(GraphUpdate update) {
  for (auto component : update.new_components) {
    add_graph_component(component);
  }
}

void Graph::override_position(PositionOverride override) {
  x[override.node.idx] = override.pos.real();
  y[override.node.idx] = override.pos.imag();
  pinned[override.node.idx] = override.pin;
}

static void repel(float *xs, float *ys, float *fxs, float *fys, size_t len) {
  float rfx_acc = 0.0, rfy_acc = 0.0;
  for (size_t i = 1; i < len; i++) {
    float dist_x = xs[i] - xs[0];
    float dist_y = ys[i] - ys[0];
    float dist_squared = dist_x * dist_x + dist_y * dist_y;
    float rcp = 1 / dist_squared;
    float force_x = dist_x * rcp;
    float force_y = dist_y * rcp;
    fxs[i] += force_x;
    fys[i] += force_y;
    rfx_acc += force_x;
    rfy_acc += force_y;
  }
  fxs[0] -= rfx_acc;
  fys[0] -= rfy_acc;
}

void Graph::repel_all() {
  float *xs = x.data();
  float *ys = y.data();
  float *fxs = fx.data();
  float *fys = fy.data();
  for (size_t len = x.size(); len > 0; --len) {
    repel(xs, ys, fxs, fys, len);
    xs++;
    ys++;
    fxs++;
    fys++;
  }
  for (auto &f : fx) {
    f *= repulse_const;
  }
  for (auto &f : fy) {
    f *= repulse_const;
  }
}

void Graph::attract_edges() {
  for (auto edge : edges) {
    auto start = node_pos(edge.start);
    auto end = node_pos(edge.end);
    auto dist = end - start;
    auto dist_squared = std::norm(dist);
    auto undo_repeal_force = repulse_const / dist_squared;
    auto spring_force =
        std::log(dist_squared / (scale_factor * scale_factor)) * spring_const;
    auto force_vec = (spring_force + undo_repeal_force) * dist;
    set_node_force(edge.start, node_force(edge.start) + force_vec);
    set_node_force(edge.end, node_force(edge.end) - force_vec);
  }
}

void Graph::gravity() {
  for (size_t i = 0; i < x.size(); i++) {
    auto dist = center - std::complex(x[i], y[i]);
    auto force_vec = gravity_const * dist;
    fx[i] += force_vec.real();
    fy[i] += force_vec.imag();
  }
}

void Graph::apply_force() {
  for (size_t i = 0; i < x.size(); i++) {
    if (pinned[i]) {
      continue;
    }
    if (std::fpclassify(fx[i]) != FP_NORMAL ||
        std::fpclassify(fy[i]) != FP_NORMAL) {
      fx[i] = fy[i] = 0;
      continue;
    }
    // prevent explosion
    fx[i] = std::max(-max_force, std::min(max_force, fx[i]));
    fy[i] = std::max(-max_force, std::min(max_force, fy[i]));
    x[i] += fx[i];
    y[i] += fy[i];
    fx[i] = fy[i] = 0;
  }
}

void Graph::update_center() {
  if (x.empty()) {
    center = 0;
    return;
  }
  auto center_x = std::accumulate(x.begin(), x.end(), 0.0f) / x.size();
  auto center_y = std::accumulate(y.begin(), y.end(), 0.0f) / y.size();
  center = {center_x, center_y};
}

void Graph::step() {
  for (int i = 0; i < 10; i++) {
    repel_all();
    attract_edges();
    gravity();
    apply_force();
    update_center();
  }
  auto update = PositionsUpdate{x, y, std::move(new_edges)};
  new_edges = {};
  emit positions_update(update);
}

GraphNodeItem::GraphNodeItem(QGraphicsItem *parent, NodeInfoProvider &provider,
                             GraphScene &scene, Node idx)
    : QGraphicsSimpleTextItem(provider.node_name(idx), parent), scene(scene),
      idx(idx), color(provider.node_color(idx)) {
  // make clickable
  setAcceptHoverEvents(true);
}

void GraphNodeItem::paint(QPainter *painter,
                          const QStyleOptionGraphicsItem *option,
                          QWidget *widget) {
  auto rect = boundingRect();
  QPen pen(Qt::black);
  if (selected) {
    pen.setColor(Qt::red);
  }
  if (pinned) {
    pen.setStyle(Qt::DotLine);
  }
  painter->setPen(std::move(pen));
  auto used_color = color;
  if (pinned) {
    used_color = used_color.lighter(120);
  }
  painter->setBrush(used_color);
  painter->drawRect(rect);
  QGraphicsSimpleTextItem::paint(painter, option, widget);
}

void GraphScene::update_positions(PositionsUpdate update) {
  for (size_t i = 0; i < nodes.size(); i++) {
    auto node = nodes[i];
    node->setCenterPos(update.x[i], update.y[i]);
  }
  auto old_nodes_size = nodes.size();
  auto selected = select->get_root();
  for (size_t i = nodes.size(); i < update.x.size(); i++) {
    auto idx = Node{i};
    auto name = info_provider.node_name(idx);
    auto node = new GraphNodeItem(nullptr, info_provider, *this, idx);
    nodes.push_back(node);
    node->setCenterPos(update.x[i], update.y[i]);
    node->setSelected(selected == idx);
    addItem(node);
  }
  for (auto [line, edge] : edges) {
    auto start = edge.start.idx;
    auto end = edge.end.idx;
    line->setLine(update.x[start], update.y[start], update.x[end],
                  update.y[end]);
  }
  for (auto edge : update.new_edges) {
    auto start = edge.start.idx;
    auto end = edge.end.idx;
    auto line = new QGraphicsLineItem(update.x[start], update.y[start],
                                      update.x[end], update.y[end]);
    line->setZValue(-1);
    edges.push_back({line, edge});
    addItem(line);
  }
  if (selected && selected->idx < nodes.size()) {
    auto selected_pos = nodes[selected->idx];
    emit selected_node_moved(selected_pos, selected->idx >= old_nodes_size);
  }
}
void GraphScene::select_node(Node idx) {
  if (nodes.size() > idx.idx) {
    nodes[idx.idx]->setSelected(true);
    emit selected_node_moved(nodes[idx.idx], true);
  }
}

GraphNodeItem *GraphScene::node(Node idx) const {
  if (idx.idx >= nodes.size()) {
    return nullptr;
  }
  return nodes[idx.idx];
}

GraphScene::GraphScene(QObject *parent, NodeInfoProvider &info_provider,
                       Graph &graph, std::shared_ptr<SelectionState> &select)
    : QGraphicsScene(parent), info_provider(info_provider), select(select) {
  connect(&graph, &Graph::positions_update, this,
          &GraphScene::update_positions);
  connect(this, &GraphScene::position_override, &graph,
          &Graph::override_position);
  connect(select.get(), &SelectionState::begin_root_change, this,
          &GraphScene::clear_selection);
  connect(select.get(), &SelectionState::root_changed, this,
          &GraphScene::select_node);
}

void GraphScene::move_node(QPointF pos, Node idx, bool pinned) {
  auto node = nodes[idx.idx];
  node->setPinned(pinned);
  node->setCenterPos(pos, true);
  complex point = {static_cast<float>(pos.x()), static_cast<float>(pos.y())};
  auto override = PositionOverride{idx, point, pinned};
  emit position_override(override);
}

void GraphNodeItem::mouseDoubleClickEvent(QGraphicsSceneMouseEvent *event) {
  scene.node_double_clicked(idx);
}

void GraphNodeItem::setCenterPos(QPointF pos, bool move_pinned) {
  if (move_pinned || !pinned) {
    setPos(pos - boundingRect().center());
  }
}

void GraphNodeItem::setCenterPos(float x, float y, bool move_pinned) {
  setCenterPos({x, y}, move_pinned);
}

QPointF GraphNodeItem::centerPos() const {
  return pos() + boundingRect().center();
}

void GraphNodeItem::setPinned(bool pinned) {
  this->pinned = pinned;
  update();
}

void GraphScene::node_double_clicked(Node node) { select->set_root(node); }

void GraphScene::clear_selection() {
  auto node = select->get_root();
  if (!node) {
    return;
  }
  auto idx = node->idx;
  if (idx >= nodes.size()) {
    return;
  }
  nodes[idx]->setSelected(false);
}
