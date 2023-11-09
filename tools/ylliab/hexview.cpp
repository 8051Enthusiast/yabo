#include "hexview.hpp"

#include "colorscrollbar.hpp"
#include "hex.hpp"

#include <QHeaderView>
#include <QTableView>

HexTableView::HexTableView(QWidget *parent) : QTableView(parent) {
  QFont hexfont("Monospace");
  hexfont.setStyleHint(QFont::TypeWriter);
  hexfont.setPointSize(12);
  setFont(hexfont);
  hexCell = std::make_unique<HexCell>(hexfont);
  setItemDelegate(hexCell.get());
}

void HexTableView::setModel(HexTableModel *model) {
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
