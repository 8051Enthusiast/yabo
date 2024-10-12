#include <QAbstractItemView>
#include <QHeaderView>
#include <QLineEdit>
#include <QMenu>
#include <QMouseEvent>
#include <QPainter>
#include <QPalette>
#include <QPoint>
#include <QShortcut>
#include <QTableView>
#include <QWidgetAction>
#include <qnamespace.h>

#include "colorscrollbar.hpp"
#include "hex.hpp"
#include "hexview.hpp"
#include "selectionstate.hpp"

HexTableView::HexTableView(QWidget *parent)
    : QTableView(parent), select(nullptr) {
  QFont hexfont("Monospace");
  hexfont.setStyleHint(QFont::TypeWriter);
  hexfont.setPointSize(12);
  setFont(hexfont);
  hexCell = std::make_unique<HexCell>(hexfont, 0);
  setItemDelegate(hexCell.get());
  connect(this, &QWidget::customContextMenuRequested, this,
          &HexTableView::context_menu);
  setContextMenuPolicy(Qt::CustomContextMenu);
  auto parse_shortcut = new QShortcut(QKeySequence("a"), this);
  connect(parse_shortcut, &QShortcut::activated,
          [this]() { parse_menu(QCursor::pos()); });
}

void HexTableView::set_model(HexTableModel *model,
                             std::shared_ptr<SelectionState> &select) {
  hexModel = model;
  hexCell->set_file_size(model->file->end_address());

  QTableView::setModel(model);
  auto scroll_bar = new ColorScrollBar(model);
  connect(scroll_bar, &ColorScrollBar::big_jump, this,
          &HexTableView::goto_addr);
  setVerticalScrollBar(scroll_bar);
  if (this->select) {
    disconnect(this->select.get(), &SelectionState::root_changed, this,
               &HexTableView::goto_node);
    disconnect(this->select.get(), &SelectionState::goto_addr, this,
               &HexTableView::goto_addr);
  }
  connect(select.get(), &SelectionState::root_changed, this,
          &HexTableView::goto_node);
  connect(select.get(), &SelectionState::goto_addr, this,
          &HexTableView::goto_addr);
  this->select = select;
  auto selection_model = new HexSelectionModel(hexModel, select);
  setSelectionModel(selection_model);
  update_dimensions();
}

void HexTableView::update_dimensions() {
  auto size = hexCell->get_cell_size();
  auto vert_header = verticalHeader();
  vert_header->setSectionResizeMode(QHeaderView::Fixed);
  vert_header->setDefaultSectionSize(size.height());
  auto header_size = hexCell->get_header_size();
  vert_header->setFixedWidth(header_size.width());
  auto horiz_header = horizontalHeader();
  horiz_header->setSectionResizeMode(QHeaderView::Fixed);
  horiz_header->setMinimumSectionSize(hexCell->get_half_cell_size().width());
  int columns = hexModel ? hexModel->bytes_per_row() : 16;
  for (int i = 0; i < columns; i++) {
    setColumnWidth(i, hexCell->get_cell_size().width());
    setColumnWidth(columns + i, hexCell->get_half_cell_size().width());
  }
}

void HexTableView::set_font(QFont font) {
  setFont(font);
  hexCell->set_font(font);
  update_dimensions();
  update();
}

void HexTableView::goto_addr(size_t addr) {
  addr = std::min(addr, hexModel->file->end_address());
  auto row = hexModel->addr_row(addr);
  hexModel->put_row_in_range(row);
  auto local_row = hexModel->local_row(row);
  auto index = hexModel->index(local_row, 0);
  scrollTo(index, QAbstractItemView::PositionAtTop);
  verticalScrollBar()->update();
}

void HexTableView::goto_node(Node node) {
  auto addr = hexModel->node_addr(node);
  if (!addr) {
    return;
  }
  goto_addr(*addr);
}

void HexTableView::context_menu(const QPoint &pos) {
  auto index = indexAt(pos);
  if (!index.isValid() || !select) {
    return;
  }
  auto menu = hexModel->create_context_menu(index, select);
  if (!menu) {
    return;
  }
  menu->popup(viewport()->mapToGlobal(pos));
}

void HexTableView::parse_menu(QPoint current_mouse_pos) {
  if (!parseRequester) {
    return;
  }
  auto index = indexAt(viewport()->mapFromGlobal(current_mouse_pos));
  if (!index.isValid()) {
    return;
  }
  auto addr = hexModel->index_addr(index);
  if (addr >= hexModel->file->end_address()) {
    return;
  }
  auto menu = new QMenu(this);
  menu->setFont(font());
  auto widget_action = new QWidgetAction(menu);
  auto line_edit = new QLineEdit(menu);
  line_edit->setFont(font());
  line_edit->setText(parseRequester->last_requested_parse());
  line_edit->setSelection(0, line_edit->text().size());
  connect(line_edit, &QLineEdit::returnPressed, this,
          [this, line_edit, menu, addr]() {
            auto name = line_edit->text();
            if (name.isEmpty()) {
              return;
            }
            menu->close();
            activateWindow();
            auto root = parseRequester->request_parse(name, addr);
            select->set_root(root);
          });
  widget_action->setDefaultWidget(line_edit);
  menu->addAction(widget_action);
  auto &recently_used = parseRequester->recently_used_funcs();
  for (auto it = recently_used.rbegin(); it != recently_used.rend(); ++it) {
    auto &name = *it;
    auto action = menu->addAction(name);
    connect(action, &QAction::triggered, this, [this, menu, name, addr]() {
      menu->close();
      activateWindow();
      auto root = parseRequester->request_parse(name, addr);
      select->set_root(root);
    });
  }
  menu->popup(current_mouse_pos);
  // make sure that line_edit gets the focus when the menu is shown
  line_edit->setFocus();
  menu->setActiveAction(widget_action);
}

void HexCell::set_file_size(size_t file_size) {
  current_file_size = file_size;
  auto metrics = QFontMetrics(font);
  auto addr_string = QString("0x%1").arg(
      file_size, address_digit_count(file_size), 16, QChar('0'));
  header_size = padded(metrics.size(0, addr_string));
}

void HexCell::set_font(QFont font) {
  this->font = font;
  auto metrics = QFontMetrics(font);
  cell_size = padded(metrics.size(0, "00"));
  half_cell_size = half_padded(metrics.size(0, "0"));
  auto addr_string = QString("0x%1").arg(current_file_size,
                                         address_digit_count(current_file_size),
                                         16, QChar('0'));
  header_size = padded(metrics.size(0, addr_string));
}

QSize HexCell::padded(QSize size) const {
  auto added_height = size.height() / 3;
  auto added_width = added_height * 4 / 3;
  return size + QSize(added_width, added_height);
}

QSize HexCell::half_padded(QSize size) const {
  auto added_height = size.height() / 3;
  auto added_width = added_height;
  return size + QSize(added_width, added_height);
}

void HexCell::paint(QPainter *painter, const QStyleOptionViewItem &option,
                    const QModelIndex &index) const {
  auto data = index.data().toString();
  painter->setPen(Qt::transparent);
  auto background = index.data(Qt::BackgroundRole);
  QBrush brush;
  if (background.isValid()) {
    brush.setColor(background.value<QColor>());
  } else {
    brush.setColor(option.palette.color(QPalette::Base));
  }
  if (option.state & (QStyle::State_Selected | QStyle::State_MouseOver)) {
    brush.setStyle(Qt::Dense4Pattern);
  } else {
    brush.setStyle(Qt::SolidPattern);
  }
  painter->setBrush(brush);
  painter->drawRect(option.rect);
  auto foreground = index.data(Qt::ForegroundRole);
  if (foreground.isValid()) {
    painter->setPen(foreground.value<QColor>());
  } else {
    painter->setPen(option.palette.color(QPalette::Text));
  }
  painter->setFont(font);
  painter->drawText(option.rect, Qt::AlignCenter, data);
  auto middle = index.model()->columnCount() / 2;
  if (index.column() == middle) {
    painter->drawLine(option.rect.topLeft(), option.rect.bottomLeft());
  }
}

QSize HexCell::sizeHint(const QStyleOptionViewItem &option,
                        const QModelIndex &index) const {
  auto len = index.data().toString().size();
  if (len == 1) {
    return half_cell_size;
  } else {
    return cell_size;
  }
}

HexCell::HexCell(QFont font, size_t file_size)
    : font(font), current_file_size(file_size) {
  auto metrics = QFontMetrics(font);
  cell_size = padded(metrics.size(0, "00"));
  half_cell_size = half_padded(metrics.size(0, "0"));
  set_file_size(file_size);
}