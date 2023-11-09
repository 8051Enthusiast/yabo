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
  auto max = this->maximum();
  auto min = this->minimum();
  auto val = this->value();
  auto size = this->size();
  auto range = (double)max - (double)min + 1;
  if (range == 0) {
    return 1;
  }
  auto offset = (double)(val - min) * (double)(size.height() - 2) / range;
  return (int)(offset + 1);
}

void ColorScrollBar::minimap_change() {
  minimap_updated = true;
  this->update();
}

// converts the position of the mouse to a value for the scrollbar
int ColorScrollBar::offset_value(int offset) const {
  auto min_pos = 1;
  auto max_pos = this->size().height() - 1;
  if (min_pos >= max_pos) {
    return this->minimum();
  }
  double clamped_offset = std::clamp(offset, min_pos, max_pos);
  if (clamped_offset == max_pos) {
    return this->maximum();
  }
  auto max = this->maximum();
  auto min = this->minimum();
  double range = max - min + 1;
  double value = (clamped_offset - min_pos) * range / (max_pos - min_pos);
  return (int)value + min;
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