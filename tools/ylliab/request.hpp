#pragma once

#include "yabo.hpp"
#include <QAbstractItemModel>
#include <QThread>
#include <optional>
#include <variant>
#include <vector>

struct TreeIndex {
  size_t idx;
  bool operator==(const TreeIndex &other) const noexcept = default;
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

struct ValHandle {
  ValHandle(YaboVal val) noexcept
      : handle(reinterpret_cast<int64_t>(val.val)) {}
  ValHandle(int64_t handle) noexcept : handle(handle) {}
  int64_t handle;
  bool operator==(const ValHandle &other) const noexcept = default;
};

template <> struct std::hash<ValHandle> {
  std::size_t operator()(const ValHandle &k) const noexcept {
    return std::hash<int64_t>()(k.handle);
  }
};

struct ValFlags {
  ValFlags() noexcept = default;
  ValFlags(YaboVal) noexcept;
  uint8_t is_list : 1;
};

struct SpannedHandle {
  SpannedHandle(SpannedVal val) noexcept;
  SpannedHandle(ValHandle val, FileSpan span, YaboValKind kind, bool active,
                ValFlags flags) noexcept
      : handle(val.handle), active(active), span(span), kind(kind),
        name(QString()), flags(flags) {}
  explicit SpannedHandle() noexcept
      : handle(1), active(false), span(FileSpan()),
        kind(YaboValKind::YABOERROR), name(QString()), flags(ValFlags()) {}
  int64_t handle;
  FileSpan span;
  QString name;
  YaboValKind kind;
  bool active;
  ValFlags flags;
  std::optional<ValHandle> access_val() const noexcept;
  std::optional<int64_t> access_error() const noexcept {
    if (kind == YaboValKind::YABOERROR) {
      return handle;
    }
    return {};
  }
  std::optional<bool> access_bool() const noexcept {
    if (kind == YaboValKind::YABOUNIT) {
      return handle == 1;
    }
    return {};
  }
  std::optional<int64_t> access_int() const noexcept {
    if (kind == YaboValKind::YABOINTEGER) {
      return handle;
    }
    return {};
  }
  std::optional<int32_t> access_char() const noexcept {
    if (kind == YaboValKind::YABOCHAR) {
      return handle;
    }
    return {};
  }
  std::optional<uint8_t> access_u8() const noexcept {
    if (kind == YaboValKind::YABOU8) {
      return handle;
    }
    return {};
  }
};

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
  Request(Meta meta, SpannedHandle value)
      : metadata(meta), val(value), array_start_index(0) {}
  Request(Meta meta, SpannedHandle value, size_t array_start_index)
      : metadata(meta), val(value), array_start_index(array_start_index) {}
  Request() : metadata(Meta()), val(SpannedHandle()) {}
  Meta metadata;
  SpannedHandle val;
  // only used to indicate the start index of an array
  // with ARRAY_ELEMENTS
  size_t array_start_index;
};

Q_DECLARE_METATYPE(Request)

struct NamedYaboVal {
  QString name;
  SpannedHandle val;
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
struct ValVecResponse {
  std::vector<NamedYaboVal> vals;
  std::optional<ValHandle> continuation;
};

struct Response {
  Response() : metadata(Meta()) {}
  Response(Meta meta, ValVecResponse &&vals)
      : metadata(meta), data(std::move(vals)) {}
  Response(Meta meta, NamedYaboVal &&val)
      : metadata(meta), data(std::move(val)) {}
  // emit an error
  Response(Meta meta) : metadata(meta) { metadata.kind = MessageType::ERROR; }
  Meta metadata;
  std::variant<ValVecResponse, NamedYaboVal> data;
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