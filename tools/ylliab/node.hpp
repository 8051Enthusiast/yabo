#pragma once
#include "request.hpp"

struct Node {
  Node(RootIndex idx) : idx(idx.root_idx) {}
  Node(size_t idx) : idx(idx) {}
  bool operator==(const Node &other) const noexcept = default;
  size_t idx;
};

struct NodeRange {
  NodeRange() : start(0), end(0), node(Node(0)) {}
  NodeRange(size_t start, size_t end, Node node)
      : start(start), end(end), node(node) {}
  size_t start;
  size_t end;
  Node node;
};

Q_DECLARE_METATYPE(NodeRange)

class NodeInfoProvider {
public:
  virtual QString node_name(Node idx) const = 0;
  virtual QColor node_color(Node idx) const = 0;
  virtual void change_root(Node idx) = 0;
  virtual std::optional<std::pair<size_t, size_t>>
  node_range(Node idx) const = 0;
};
