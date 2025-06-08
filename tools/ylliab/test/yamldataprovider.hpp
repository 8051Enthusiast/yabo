#pragma once
#include "dataprovider.hpp"
#include <yaml-cpp/yaml.h>
#include <unordered_map>
#include <vector>

// YAML-based data provider for testing
// Loads pre-computed model data from YAML files instead of running actual parsers
// Supports circular data structures using YAML anchors and aliases
class YamlDataProvider : public DataProvider {
  Q_OBJECT
public:
  explicit YamlDataProvider(const QString& yaml_path);
  ~YamlDataProvider() = default;

public slots:
  void execute_request_slot(Request req) override;
  void execute_parser_slot(Meta meta, QString func_name, size_t pos) override;

private:
  struct ParsedParser {
    QString name;
    size_t pos;
    YAML::Node root;
  };

  void load_yaml(const QString& yaml_path);
  SpannedHandle create_handle_from_yaml_node(const YAML::Node& node);
  YaboValKind string_to_kind(const QString& type_str);
  Response create_fields_response(const Request& req, const YAML::Node& node);
  Response create_array_response(const Request& req, const YAML::Node& node);
  Response create_parse_response(const Meta& meta, const QString& func_name, size_t pos);
  QString get_yaml_string(const YAML::Node& node, const std::string& key, const QString& default_value = "");
  template<typename T> T get_yaml_value(const YAML::Node& node, const std::string& key, const T& default_value);

  YAML::Node document;
  std::vector<ParsedParser> parsers;
  std::unordered_map<int64_t, YAML::Node> handle_to_yaml_node;
};