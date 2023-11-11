#pragma once

#include "hex.hpp"

#include <QTableView>

class HexTableView : public QTableView {
  Q_OBJECT
public:
  HexTableView(QWidget *parent = nullptr);
  void setModel(HexTableModel *model);

public slots:
  void goto_addr(size_t addr);
  void goto_node(Node node);

private:
  HexTableModel *hexModel;
  std::unique_ptr<HexCell> hexCell;
};
