#include "hex.hpp"
#include <QPainter>
#include <QPixmap>
#include <qimage.h>
#include <qnamespace.h>
#include <vector>

QVariant HexTableModel::data(const QModelIndex &index, int role) const {
  auto row = index.row();
  auto col = index.column();
  auto offset = row * columns + col;
  if (role == Qt::DisplayRole) {
    if (offset >= file->span().size()) {
      return QVariant();
    }
    return QString("%1").arg(file->span()[offset], 2, 16, QChar('0'));
  } else if (role == Qt::BackgroundRole) {
    auto node = ranges.get(offset);
    if (!node) {
      return QColor(Qt::transparent);
    }
    return node_info->node_color(node->node);
  } else {
    return QVariant();
  }
}

QVariant HexTableModel::headerData(int section, Qt::Orientation orientation,
                                   int role) const {
  if (role == Qt::BackgroundRole) {
    return QColor(Qt::lightGray);
  }
  if (role == Qt::DisplayRole && orientation == Qt::Vertical) {
    return QString("0x%1").arg(section * columns, 8, 16, QChar('0'));
  } else {
    return QVariant();
  }
}

void HexTableModel::add_range(NodeRange range) {
  ranges.insert(range);
  size_t start_row = range.start / columns;
  size_t end_row = (range.end - 1) / columns;
  emit dataChanged(createIndex(start_row, 0),
                   createIndex(end_row, columns - 1));
  emit updated_minimap();
}

std::optional<size_t> HexTableModel::node_addr(Node node) const {
  auto range = node_info->node_range(node);
  if (!range) {
    return {};
  }
  return range->first;
}

int HexTableModel::addr_row(size_t address) const {
  return address / columns;
}

size_t HexTableModel::row_addr(int row) const {
  return (size_t)row * columns;
}

std::pair<size_t, size_t> HexTableModel::pixel_offset_addr_range(int offset,
                                                          int max) const {
  auto size = file->span().size();
  auto start_ratio = (double)offset / max;
  auto end_ratio = (double)(offset + 1) / max;
  auto start = (size_t)(size * start_ratio);
  auto end = (size_t)(size * end_ratio);
  return {start, end};
}

int HexTableModel::addr_pixel_offset(size_t addr, int max) const {
  auto size = file->span().size();
  auto ratio = (double)addr / size;
  auto offset = (int)(ratio * max);
  return offset;
}

void HexTableModel::handle_doubleclick(const QModelIndex &index) {
  auto row = index.row();
  auto col = index.column();
  auto offset = row * columns + col;
  auto node = ranges.get(offset);
  if (!node) {
    return;
  }
  node_info->change_root(node->node);
}

QPixmap HexTableModel::node_minimap(int len) const {
  QImage image(1, len + 2, QImage::Format_RGB32);
  auto default_color = QColor(Qt::white).rgb();
  image.setPixel(0, 0, default_color);
  image.setPixel(0, len + 1, default_color);
  for (size_t i = 0; i < len; i++) {
    auto [min_offset, max_offset] = pixel_offset_addr_range(i, len);
    auto node = ranges.get_next(min_offset);
    if (!node) {
      image.setPixel(0, i + 1, default_color);
      continue;
    }
    if (node->start > max_offset) {
      image.setPixel(0, i + 1, default_color);
      continue;
    }
    auto node_color = node_info->node_color(node->node);
    image.setPixel(0, i + 1, node_color.rgb());
  }
  return QPixmap::fromImage(image);
}

void HexCell::paint(QPainter *painter, const QStyleOptionViewItem &option,
                    const QModelIndex &index) const {
  auto data = index.data().toString();
  painter->save();
  painter->setPen(Qt::transparent);
  auto background = index.data(Qt::BackgroundRole).value<QColor>();
  painter->setBrush(background);
  painter->drawRect(option.rect);
  painter->setFont(font);
  painter->setPen(Qt::black);
  painter->drawText(option.rect, Qt::AlignCenter, data);
  painter->restore();
}

QSize HexCell::sizeHint(const QStyleOptionViewItem &option,
                        const QModelIndex &index) const {

  return cell_size;
}

HexCell::HexCell(QFont font) : font(font) {
  auto metrics = QFontMetrics(font);
  cell_size = metrics.size(0, "00");
  cell_size.setHeight(cell_size.height() + 6);
  cell_size.setWidth(cell_size.width() + 10);
}
