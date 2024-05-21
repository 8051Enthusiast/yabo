#include "rangemap.hpp"

std::pair<RangeMap::MapEntry *, RangeMap::MapEntry *>
RangeMap::split_entry(MapEntry *entry, size_t middle) {
  auto new_entry = ranges.insert({middle, *entry}).first;
  new_entry->second.start = middle;
  entry->end = middle;
  return {entry, &new_entry->second};
}

RangeMap::MapEntry const *RangeMap::containing_entry(size_t index) const {
  auto it = ranges.upper_bound(index);
  if (it == ranges.begin()) {
    return nullptr;
  }
  --it;
  if (it->second.end <= index) {
    return nullptr;
  }
  return &it->second;
}

RangeMap::MapEntry *RangeMap::containing_entry(size_t index) {
  auto const_this = const_cast<RangeMap const *>(this);
  return const_cast<RangeMap::MapEntry *>(const_this->containing_entry(index));
}

void RangeMap::insert(size_t start, size_t end, Node value) {
  if (start == end) {
    return;
  }
  assert(start < end);
  NodeRange val(start, end, value);
  auto current = containing_entry(start);
  if (current && current->start < start) {
    // we split it here so that the latter half will get
    // updated with the current range later in the loop
    split_entry(current, start);
  }
  auto it = ranges.lower_bound(start);
  while (it != ranges.end() && it->first < end) {
    // the little known barbed arrow operator,,,
    current = &it++->second;
    // there is space before the next range, so we need to
    // insert our ranges for that section
    if (start < current->start) {
      auto new_entry = RangeMap::MapEntry(start, current->start, {val});
      ranges.insert({start, new_entry});
    }
    if (end < current->end) {
      // our range ends earlier than the current range in the map
      // so we split it up and just insert our value into the first
      // half
      // note that the iteration ends after this loop if
      // we need to split
      split_entry(current, end);
    }
    current->value.push_back(val);
    start = current->end;
  }
  // if there is still space left (or if there were no elements
  // in the map in the first place), we insert the remaining
  // range
  if (start < end) {
    auto new_entry = RangeMap::MapEntry(start, end, {val});
    ranges.insert({start, new_entry});
  }
}

std::optional<Node> min_sized(std::vector<NodeRange> const &vals) {
  if (vals.empty()) {
    return {};
  }
  auto min = vals[0];
  for (auto &val : vals) {
    if (val.end - val.start <= min.end - min.start) {
      min = val;
    }
  }
  return min.node;
}

std::optional<NodeRange> RangeMap::get(size_t index) const {
  auto current = containing_entry(index);
  if (!current) {
    return {};
  }
  auto min = min_sized(current->value);
  if (!min) {
    return {};
  }
  return NodeRange{current->start, current->end, *min};
}

std::optional<NodeRange> RangeMap::get_next(size_t index) const {
  auto node = get(index);
  if (node) {
    return node;
  }
  auto it = ranges.upper_bound(index);
  if (it == ranges.end()) {
    return {};
  }
  auto &entry = it->second;
  auto min = min_sized(entry.value);
  if (!min) {
    return {};
  }
  return NodeRange{entry.start, entry.end, *min};
};