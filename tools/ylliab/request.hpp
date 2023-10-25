#pragma once

#include "yabo.hpp"
#include <QAbstractItemModel>
#include <QThread>
#include <variant>
#include <vector>

struct TreeIndex {
  size_t idx;
  bool operator==(const TreeIndex &other) const noexcept = default;
};

template <> struct std::hash<TreeIndex> {
  std::size_t operator()(const TreeIndex &k) const noexcept {
    return std::hash<size_t>()(k.idx);
  }
};

constexpr TreeIndex INVALID_PARENT = TreeIndex{(size_t)-1};

enum class MessageType {
  FIELDS,
  ARRAY_ELEMENTS,
  DEREF,
  PARSE,
  ERROR,
};

struct Meta {
  Meta()
      : idx(INVALID_PARENT), kind(MessageType::ERROR), root(INVALID_PARENT) {}
  Meta(TreeIndex idx, MessageType kind, TreeIndex root)
      : idx(idx), kind(kind), root(root) {}
  TreeIndex idx;
  MessageType kind;
  TreeIndex root;
};

Q_DECLARE_METATYPE(Meta)

struct Request {
  Request(Meta meta, SpannedVal value) : metadata(meta), val(value) {}
  Request() : metadata(Meta()), val(YaboVal(nullptr)) {}
  Meta metadata;
  SpannedVal val;
};

Q_DECLARE_METATYPE(Request)

typedef std::vector<std::pair<std::string, SpannedVal>> YaboValVec;

struct Response {
  Response() : metadata(Meta()) {}
  Response(Meta meta, YaboValVec &&vals)
      : metadata(meta), data(std::move(vals)) {}
  Response(Meta meta, SpannedVal val) : metadata(meta), data(val) {}
  // emit an error
  Response(Meta meta) : metadata(meta) { metadata.kind = MessageType::ERROR; }
  Meta metadata;
  std::variant<YaboValVec, SpannedVal> data;
};

Q_DECLARE_METATYPE(Response)
