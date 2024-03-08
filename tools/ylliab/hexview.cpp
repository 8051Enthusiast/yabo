#include "hexview.hpp"

#include "colorscrollbar.hpp"
#include "hex.hpp"

#include <QAbstractItemView>
#include <QHeaderView>
#include <QTableView>
#include <qpalette.h>

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
  auto size = hexCell->get_cell_size();
  auto vert_header = verticalHeader();
  vert_header->setMinimumSectionSize(size.height());
  vert_header->setSectionResizeMode(QHeaderView::Fixed);
  vert_header->setDefaultSectionSize(size.height());
  vert_header->setFixedWidth(hexCell->get_header_size().width());
  auto horiz_header = horizontalHeader();
  horiz_header->setMinimumSectionSize(size.width());
  horiz_header->setSectionResizeMode(QHeaderView::Fixed);
  horiz_header->setDefaultSectionSize(size.width());
  QTableView::setModel(model);
  auto scroll_bar = new ColorScrollBar(model);
  connect(scroll_bar, &ColorScrollBar::big_jump, this,
          &HexTableView::goto_addr);
  setVerticalScrollBar(scroll_bar);
}

void HexTableView::goto_addr(size_t addr) {
  auto row = hexModel->addr_row(addr);
  hexModel->put_row_in_range(row);
  auto local_row = hexModel->local_row(row);
  auto index = hexModel->index(local_row, 0);
  scrollTo(index, QAbstractItemView::PositionAtTop);
  verticalScrollBar()->update();
}

void HexTableView::goto_node(Node node) {
  auto addr = hexModel->node_addr(node);
  goto_addr(*addr);
}

void HexTableView::select_addr_range(size_t start, size_t end) {
  selectionModel()->clearSelection();
  if (start == end) {
    return;
  }
  auto start_row = hexModel->addr_row(start);
  auto last_row = hexModel->addr_row(end - 1);
  auto start_local_row = hexModel->local_row(start_row);
  auto last_local_row = hexModel->local_row(last_row);
  auto col = hexModel->columnCount();
  auto start_column = start % col;
  auto last_column = (end - 1) % col;
  auto selection = QItemSelection();
  if (start_local_row == last_local_row) {
    selection.select(hexModel->index(start_local_row, start_column),
                     hexModel->index(start_local_row, last_column));
  } else {
    selection.select(hexModel->index(start_local_row, start_column),
                     hexModel->index(start_local_row, col - 1));
    if (last_local_row - start_local_row >= 2) {
      selection.select(hexModel->index(start_local_row + 1, 0),
                       hexModel->index(last_local_row - 1, col - 1));
    }
    selection.select(hexModel->index(last_local_row, 0),
                     hexModel->index(last_local_row, last_column));
  }
  selectionModel()->select(selection, QItemSelectionModel::Select);
}