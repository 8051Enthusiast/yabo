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

constexpr TreeIndex INVALID_PARENT = TreeIndex{(size_t)-1};

enum class MessageType {
  FIELDS,
  ARRAY_ELEMENTS,
  PARSE,
  ERROR,
};

struct Meta {
  TreeIndex idx;
  MessageType kind;
  void *user_data;
};

Q_DECLARE_METATYPE(Meta)

struct Request {
  Request(Meta meta, YaboVal value) : metadata(meta), val(value) {}
  Request() : metadata(Meta{INVALID_PARENT, MessageType::ERROR, nullptr}), val(YaboVal(nullptr)) {}
  Meta metadata;
  YaboVal val;
};

Q_DECLARE_METATYPE(Request)

typedef std::vector<std::pair<std::string, YaboVal>> YaboValVec;

struct Response {
  Response() : metadata(Meta{INVALID_PARENT, MessageType::ERROR, nullptr}) {}
  Response(Meta meta, YaboValVec &&vals)
      : metadata(meta), data(std::move(vals)) {}
  Response(Meta meta, YaboVal val) : metadata(meta), data(val) {}
  // emit an error
  Response(Meta meta) : metadata(meta) { metadata.kind = MessageType::ERROR; }
  Meta metadata;
  std::variant<YaboValVec, YaboVal> data;
};

Q_DECLARE_METATYPE(Response)
