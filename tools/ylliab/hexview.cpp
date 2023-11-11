#include "hexview.hpp"

#include "colorscrollbar.hpp"
#include "hex.hpp"

#include <QHeaderView>
#include <QTableView>
#include <QAbstractItemView>

HexTableView::HexTableView(QWidget *parent) : QTableView(parent) {
  QFont hexfont("Monospace");
  hexfont.setStyleHint(QFont::TypeWriter);
  hexfont.setPointSize(12);
  setFont(hexfont);
  hexCell = std::make_unique<HexCell>(hexfont);
  setItemDelegate(hexCell.get());
}

void HexTableView::setModel(HexTableModel *model) {
  hexModel = model;
  QTableView::setModel(model);
  setVerticalScrollBar(new ColorScrollBar(model));
  auto size = hexCell->get_cell_size();
  auto vert_header = verticalHeader();
  vert_header->setMinimumSectionSize(size.height());
  vert_header->setSectionResizeMode(QHeaderView::Fixed);
  vert_header->setDefaultSectionSize(size.height());
  auto horiz_header = horizontalHeader();
  horiz_header->setMinimumSectionSize(size.width());
  horiz_header->setSectionResizeMode(QHeaderView::Fixed);
  horiz_header->setDefaultSectionSize(size.width());
}

void HexTableView::goto_addr(size_t addr) {
  auto row = hexModel->addr_row(addr);
  auto index = hexModel->index(row, 0);
  scrollTo(index, QAbstractItemView::PositionAtTop);
}

void HexTableView::goto_node(Node node) {
  auto addr = hexModel->node_addr(node);  
  goto_addr(*addr);
}
