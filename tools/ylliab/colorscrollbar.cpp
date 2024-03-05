#include "colorscrollbar.hpp"
#include "hex.hpp"
#include <QMouseEvent>
#include <QPainter>
#include <qnamespace.h>
#include <qpalette.h>

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
  auto val = this->value();
  auto val_row = model->global_row(val);
  auto size = this->size();
  auto offset = model->row_pixel_offset(val_row, size.height() - 2);
  return offset + 1;
}

void ColorScrollBar::minimap_change() {
  minimap_updated = true;
  this->update();
}

void ColorScrollBar::refresh_minimap() {
  auto size = this->size().height();
  if (!minimap || size != minimap.size().height() || minimap_updated) {
    minimap_updated = false;
    minimap = model->node_minimap(size, this->palette().color(QPalette::Base));
  }
}

void ColorScrollBar::paintEvent(QPaintEvent *event) {
  refresh_minimap();
  QPainter painter(this);
  auto size = this->size();
  auto border = this->palette().color(QPalette::Shadow);
  painter.setPen(border);
  painter.setBrush(minimap);
  painter.drawRect(0, 0, size.width() - 1, size.height() - 1);
  painter.setPen(Qt::red);
  auto offset = marker_offset();
  painter.drawRect(1, offset, size.width() - 3, 0);
}

// directly move to the position where the mouse is at
void ColorScrollBar::mousePressEvent(QMouseEvent *event) {
  auto offset = event->pos().y() - 1;
  set_val(offset);
}

void ColorScrollBar::mouseMoveEvent(QMouseEvent *event) {
  auto offset = event->pos().y() - 1;
  set_val(offset);
}
void ColorScrollBar::set_val(int offset) {
  auto min_pos = 1;
  auto max_pos = this->size().height() - 1;
  auto clamped_offset = std::clamp(offset, min_pos, max_pos);
  auto offset_range = max_pos - min_pos;
  auto [row, _] =
      model->pixel_offset_global_row_range(clamped_offset - min_pos, offset_range);
  if (model->row_is_in_range(row)) {
    auto local_row = model->local_row(row);
    this->setValue(local_row);
    return;
  }
  emit big_jump(model->row_addr(row));
}
