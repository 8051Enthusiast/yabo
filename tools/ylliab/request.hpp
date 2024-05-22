#pragma once

#include "yabo.hpp"
#include <QAbstractItemModel>
#include <QThread>
#include <variant>
#include <vector>

struct TreeIndex {
  size_t idx;
  bool operator==(const TreeIndex &other) const noexcept = default;
  TreeIndex operator++() noexcept {
    idx++;
    return *this;
  }
};

Q_DECLARE_METATYPE(TreeIndex)

template <> struct std::hash<TreeIndex> {
  std::size_t operator()(const TreeIndex &k) const noexcept {
    return std::hash<size_t>()(k.idx);
  }
};

constexpr TreeIndex INVALID_PARENT = TreeIndex{(size_t)-1};

struct RootIndex {
  RootIndex() = default;
  RootIndex(TreeIndex idx, size_t root_idx)
      : tree_index(idx), root_idx(root_idx) {}
  bool operator==(const RootIndex &other) const noexcept = default;
  TreeIndex tree_index;
  size_t root_idx;
};

Q_DECLARE_METATYPE(RootIndex)

enum class MessageType {
  FIELDS,
  ARRAY_ELEMENTS,
  DEREF,
  PARSE,
  ERROR,
};

struct Meta {
  Meta()
      : idx(INVALID_PARENT), kind(MessageType::ERROR),
        root(RootIndex(TreeIndex{INVALID_PARENT}, (size_t)-1)) {}
  Meta(TreeIndex idx, MessageType kind, RootIndex root)
      : idx(idx), kind(kind), root(root) {}
  TreeIndex idx;
  MessageType kind;
  RootIndex root;
};

Q_DECLARE_METATYPE(Meta)

struct Request {
  Request(Meta meta, SpannedVal value)
      : metadata(meta), val(value), array_start_index(0) {}
  Request(Meta meta, SpannedVal value, size_t array_start_index)
      : metadata(meta), val(value), array_start_index(array_start_index) {}
  Request() : metadata(Meta()), val(SpannedVal()) {}
  Meta metadata;
  SpannedVal val;
  // only used to indicate the start index of an array
  // with ARRAY_ELEMENTS
  size_t array_start_index;
};

Q_DECLARE_METATYPE(Request)

struct NamedYaboVal {
  QString name;
  SpannedVal val;
  std::strong_ordering operator<=>(const NamedYaboVal &other) const {
    auto active_cmp = !val.active <=> !other.val.active;
    if (active_cmp != 0) {
      return active_cmp;
    }
    auto start_cmp = val.span.data() <=> other.val.span.data();
    if (start_cmp != 0) {
      return start_cmp;
    }
    auto len_cmp = -val.span.size() <=> -other.val.span.size();
    if (len_cmp != 0) {
      return len_cmp;
    }
    if (name < other.name) {
      return std::strong_ordering::less;
    }
    if (name > other.name) {
      return std::strong_ordering::greater;
    }
    return std::strong_ordering::equal;
  }
};
using YaboValVec = std::vector<NamedYaboVal>;

struct Response {
  Response() : metadata(Meta()) {}
  Response(Meta meta, YaboValVec &&vals)
      : metadata(meta), data(std::move(vals)) {}
  Response(Meta meta, NamedYaboVal &&val)
      : metadata(meta), data(std::move(val)) {}
  // emit an error
  Response(Meta meta) : metadata(meta) { metadata.kind = MessageType::ERROR; }
  Meta metadata;
  std::variant<YaboValVec, NamedYaboVal> data;
};

Q_DECLARE_METATYPE(Response)

class ParseRequester {
public:
  virtual ~ParseRequester() = default;
  void request_parse(QString func_name, size_t pos);
  QString last_requested_parse() const { return last_parse; }
  std::vector<QString> const &recently_used_funcs() const;

protected:
  virtual void run_parse(QString func_name, size_t pos) = 0;

private:
  void add_recently_used(QString func_name);
  QString last_parse;
  static constexpr size_t recently_used_list_size = 10;
  std::vector<QString> recently_used = {};
};