#pragma once
#include "node.hpp"
#include <map>
#include <optional>

class RangeMap {
public:
  RangeMap() = default;

  void insert(size_t start, size_t end, Node value);
  void insert(NodeRange range) { insert(range.start, range.end, range.node); }
  std::optional<NodeRange> get(size_t index) const;
  std::optional<NodeRange> get_next(size_t index) const;

private:
  struct MapEntry {
    size_t start;
    size_t end;
    std::vector<Node> value;
  };
  std::map<size_t, MapEntry> ranges;
  MapEntry *containing_entry(size_t index);
  MapEntry const *containing_entry(size_t index) const;
  std::pair<MapEntry *, MapEntry *> split_entry(MapEntry *entry, size_t index);
};