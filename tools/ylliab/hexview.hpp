#pragma once

#include "hex.hpp"
#include "request.hpp"

#include <QFont>
#include <QPoint>
#include <QTableView>

class HexTableView : public QTableView {
  Q_OBJECT
public:
  HexTableView(QWidget *parent = nullptr);
  void setModel(HexTableModel *model);
  void set_font(QFont font);
  void set_parser_requester(ParseRequester *parseRequester) {
    this->parseRequester = parseRequester;
  }
  void parse_menu(QPoint pos);

public slots:
  void goto_addr(size_t addr);
  void goto_node(Node node);
  void select_addr_range(size_t start, size_t end);
  void context_menu(const QPoint &pos);

private:
  void update_dimensions();
  HexTableModel *hexModel;
  ParseRequester *parseRequester = nullptr;
  std::unique_ptr<HexCell> hexCell;
};
