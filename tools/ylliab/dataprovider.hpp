#pragma once
#include <QObject>
#include "request.hpp"

// Abstract base class for providing data to FileRequester
// This allows both real Executor and YAML-based implementations
class DataProvider : public QObject {
  Q_OBJECT
public:
  virtual ~DataProvider() = default;

public slots:
  virtual void execute_request_slot(Request req) = 0;
  virtual void execute_parser_slot(Meta meta, QString func_name, size_t pos) = 0;

signals:
  void response(Response resp);
};