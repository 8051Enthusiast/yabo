#pragma once

#include "hex.hpp"

#include <QFont>
#include <QTableView>

class HexTableView : public QTableView {
  Q_OBJECT
public:
  HexTableView(QWidget *parent = nullptr);
  void setModel(HexTableModel *model);
  void set_font(QFont font);

public slots:
  void goto_addr(size_t addr);
  void goto_node(Node node);
  void select_addr_range(size_t start, size_t end);

private:
  void update_dimensions();
  HexTableModel *hexModel;
  std::unique_ptr<HexCell> hexCell;
};
