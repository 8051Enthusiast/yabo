#include "colorscrollbar.hpp"
#include "hex.hpp"
#include <QMouseEvent>
#include <QPainter>
#include <qnamespace.h>

ColorScrollBar::ColorScrollBar(HexTableModel *model, QWidget *parent)
    : QScrollBar(parent), model(model) {
  connect(model, &HexTableModel::updated_minimap, this,
          &ColorScrollBar::minimap_change);
}

// calculates the offset of the marker on the scrollbar
// the highest position is one pixel below the top, the lowest is one pixel
// above the bottom
int ColorScrollBar::marker_offset() const {
  assert(this->minimum() == 0);
  auto max = this->maximum();
  auto val = this->value();
  auto size = this->size();
  auto addr = model->row_addr(val);
  auto offset = model->addr_pixel_offset(addr, size.height() - 2);
  return offset + 1;
}

void ColorScrollBar::minimap_change() {
  minimap_updated = true;
  this->update();
}

// converts the position of the mouse to a value for the scrollbar
int ColorScrollBar::offset_value(int offset) const {
  auto min_pos = 1;
  auto max_pos = this->size().height() - 1;
  auto clamped_offset = std::clamp(offset, min_pos, max_pos);
  auto offset_range = max_pos - min_pos;
  auto [addr, _] = model->pixel_offset_addr_range(clamped_offset - min_pos, offset_range);
  auto row = model->addr_row(addr);
  return row;
}

void ColorScrollBar::refresh_minimap() {
  auto size = this->size().height();
  if (!minimap || size != minimap.size().height() || minimap_updated) {
    minimap_updated = false;
    minimap = model->node_minimap(size);
  }
}

void ColorScrollBar::paintEvent(QPaintEvent *event) {
  refresh_minimap();
  QPainter painter(this);
  painter.setPen(Qt::gray);
  painter.setBrush(minimap);
  auto size = this->size();
  painter.drawRect(0, 0, size.width() - 1, size.height() - 1);
  painter.setPen(Qt::red);
  auto offset = marker_offset();
  painter.drawRect(1, offset, size.width() - 3, 0);
}

// directly move to the position where the mouse is at
void ColorScrollBar::mousePressEvent(QMouseEvent *event) {
  auto offset = event->pos().y();
  auto value = offset_value(offset);
  this->setValue(value);
}

void ColorScrollBar::mouseMoveEvent(QMouseEvent *event) {
  auto offset = event->pos().y();
  auto value = offset_value(offset);
  this->setValue(value);
}
