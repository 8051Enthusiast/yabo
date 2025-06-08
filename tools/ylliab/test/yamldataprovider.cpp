#include "yamldataprovider.hpp"
#include <QDebug>
#include <QFile>

YamlDataProvider::YamlDataProvider(const QString &yaml_path) {
  load_yaml(yaml_path);
}

void YamlDataProvider::execute_request_slot(Request req) {
  auto handle_it = handle_to_yaml_node.find(req.val.handle);
  if (handle_it == handle_to_yaml_node.end()) {
    emit response(Response(req.metadata));
    return;
  }

  const auto &node = handle_it->second;
  Response resp;

  switch (req.metadata.kind) {
  case MessageType::FIELDS:
    resp = create_fields_response(req, node);
    break;
  case MessageType::ARRAY_ELEMENTS:
    resp = create_array_response(req, node);
    break;
  case MessageType::DEREF:
    resp = Response(req.metadata,
                    NamedYaboVal{get_yaml_string(node, "name"), create_handle_from_yaml_node(node)});
    break;
  default:
    resp = Response(req.metadata);
    break;
  }

  emit response(resp);
}

void YamlDataProvider::execute_parser_slot(Meta meta, QString func_name,
                                           size_t pos) {
  Response resp = create_parse_response(meta, func_name, pos);
  emit response(resp);
}

void YamlDataProvider::load_yaml(const QString &yaml_path) {
  document = YAML::LoadFile(yaml_path.toStdString());

  if (document["parsers"]) {
    for (const auto &parser_node : document["parsers"]) {
      ParsedParser parser;
      parser.name = get_yaml_string(parser_node, "name");
      parser.pos = get_yaml_value<size_t>(parser_node, "pos", 0);

      if (parser_node["root"]) {
        parser.root = parser_node["root"];
      }

      parsers.push_back(parser);
    }
  }
}

template<typename T>
T YamlDataProvider::get_yaml_value(const YAML::Node &node, const std::string &key, const T &default_value) {
  return node[key] ? node[key].as<T>() : default_value;
}

QString YamlDataProvider::get_yaml_string(const YAML::Node &node, const std::string &key, const QString &default_value) {
  if (!node[key]) {
    if (default_value.isEmpty()) {
      throw std::runtime_error("Required field '" + key + "' not found in YAML node");
    }
    return default_value;
  }
  return QString::fromStdString(node[key].as<std::string>());
}

SpannedHandle YamlDataProvider::create_handle_from_yaml_node(const YAML::Node &node) {
  FileSpan span;
  size_t span_size = get_yaml_value<size_t>(node, "span_size", 0);
  if (span_size > 0) {
    size_t span_start = get_yaml_value<size_t>(node, "span_start", 0);
    span = FileSpan(span_start, span_size);
  }

  QString type_str = get_yaml_string(node, "type");
  YaboValKind kind = string_to_kind(type_str);
  ValFlags flags;
  int64_t handle_value;
  bool active = get_yaml_value<bool>(node, "active", true);

  flags.is_list = 0;

  switch (kind) {
  case YaboValKind::YABOINTEGER: {
    QString value = get_yaml_string(node, "value", "0");
    handle_value = value.toLongLong();
    break;
  }
  case YaboValKind::YABOU8: {
    QString value = get_yaml_string(node, "value", "0");
    if (value.startsWith("0x") || value.startsWith("0X")) {
      handle_value = value.mid(2).toUInt(nullptr, 16);
    } else {
      handle_value = value.toUInt();
    }
    break;
  }
  case YaboValKind::YABOCHAR: {
    QString value = get_yaml_string(node, "value", "");
    handle_value = value.isEmpty() ? 0 : value.at(0).unicode();
    break;
  }
  case YaboValKind::YABOBIT: {
    QString value = get_yaml_string(node, "value", "false");
    handle_value = (value.toLower() == "true" || value == "1") ? 1 : 0;
    break;
  }
  default: {
    handle_to_yaml_node[handle_value] = node;
    break;
  }
  }

  return SpannedHandle(ValHandle(handle_value), span, kind, active, flags);
}

YaboValKind YamlDataProvider::string_to_kind(const QString &type_str) {
  if (type_str == "integer")
    return YaboValKind::YABOINTEGER;
  if (type_str == "u8")
    return YaboValKind::YABOU8;
  if (type_str == "char")
    return YaboValKind::YABOCHAR;
  if (type_str == "bit")
    return YaboValKind::YABOBIT;
  if (type_str == "array")
    return YaboValKind::YABOARRAY;
  if (type_str == "block")
    return YaboValKind::YABOBLOCK;
  if (type_str == "nominal")
    return YaboValKind::YABONOM;
  if (type_str == "unit")
    return YaboValKind::YABOUNIT;
  if (type_str == "error")
    return YaboValKind::YABOERROR;
  return YaboValKind::YABOBLOCK;
}

Response YamlDataProvider::create_fields_response(const Request &req, const YAML::Node &node) {
  std::vector<NamedYaboVal> fields;

  if (node["children"]) {
    for (const auto &child : node["children"]) {
      SpannedHandle child_handle = create_handle_from_yaml_node(child);
      QString child_name = get_yaml_string(child, "name");
      fields.push_back(NamedYaboVal{child_name, child_handle});
    }
  }

  ValVecResponse vec_resp;
  vec_resp.vals = std::move(fields);
  return Response(req.metadata, std::move(vec_resp));
}

Response YamlDataProvider::create_array_response(const Request &req, const YAML::Node &node) {
  std::vector<NamedYaboVal> elements;

  if (node["children"]) {
    size_t start_idx = req.array_start_index;
    size_t i = 0;
    for (const auto &child : node["children"]) {
      if (i >= start_idx) {
        SpannedHandle child_handle = create_handle_from_yaml_node(child);
        QString element_name = QString("[%1]").arg(i);
        elements.push_back(NamedYaboVal{element_name, child_handle});
      }
      ++i;
    }
  }

  ValVecResponse vec_resp;
  vec_resp.vals = std::move(elements);

  return Response(req.metadata, std::move(vec_resp));
}

Response YamlDataProvider::create_parse_response(const Meta &meta, const QString &func_name, size_t pos) {
  for (const auto &parser : parsers) {
    if (parser.name == func_name && parser.pos == pos) {
      SpannedHandle root_handle = create_handle_from_yaml_node(parser.root);
      return Response(meta, NamedYaboVal{parser.name, root_handle});
    }
  }

  return Response(meta);
}