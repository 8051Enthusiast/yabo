#pragma once

#include "hex.hpp"

#include <QTableView>

class HexTableView : public QTableView {
  Q_OBJECT
public:
  HexTableView(QWidget *parent = nullptr);
  void setModel(HexTableModel *model);

private:
  std::unique_ptr<HexTableModel> hexModel;
  std::unique_ptr<HexCell> hexCell;
};