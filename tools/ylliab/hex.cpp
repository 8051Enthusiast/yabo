#include "hex.hpp"
#include <QPainter>
#include <QPixmap>
#include <qimage.h>
#include <qnamespace.h>
#include <qstyleditemdelegate.h>
#include <vector>

QVariant HexTableModel::data(const QModelIndex &index, int role) const {
  auto row = index.row();
  auto col = index.column();
  auto offset = row_addr(global_row(row)) + col;
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
  } else if (role == Qt::ForegroundRole) {
    auto node = ranges.get(offset);
    if (node) {
      return QColor(Qt::black);
    }
    return QVariant();
  } else {
    return QVariant();
  }
}

QVariant HexTableModel::headerData(int section, Qt::Orientation orientation,
                                   int role) const {
  if (role == Qt::TextAlignmentRole) {
    return Qt::AlignCenter;
  }
  if (role == Qt::BackgroundRole) {
    return QColor(Qt::lightGray);
  }
  if (role == Qt::DisplayRole && orientation == Qt::Vertical) {
    return QString("0x%1").arg(row_addr(global_row(section)), 16, 16,
                               QChar('0'));
  } else {
    return QVariant();
  }
}

void HexTableModel::add_range(NodeRange range) {
  ranges.insert(range);
  auto start_row = local_row(addr_row(range.start));
  auto end_row = local_row(addr_row(range.end - 1));
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

GlobalRow HexTableModel::addr_row(size_t address) const {
  return GlobalRow{address / columns};
}

size_t HexTableModel::row_addr(GlobalRow row) const {
  return row.row * columns;
}

std::pair<GlobalRow, GlobalRow>
HexTableModel::pixel_offset_global_row_range(int offset, int max) const {
  uint64_t size = global_row_count();
  auto start = size * offset / max;
  auto end = size * (offset + 1) / max;
  return {GlobalRow{(size_t)start}, GlobalRow{(size_t)end}};
}

int HexTableModel::row_pixel_offset(GlobalRow row, int max) const {
  uint64_t size = global_row_count();
  auto roundup_margin = std::min((uint64_t)max, size) - 1;
  auto multiple = (uint64_t)max * row.row + roundup_margin;
  auto offset = multiple / size;
  return offset;
}

bool HexTableModel::row_is_in_range(GlobalRow row) const {
  uint64_t start = 0;
  if (model_offset != 0) {
    start = (uint64_t)model_offset + min_view_offset;
  }

  uint64_t end = global_row_count();
  if (end - model_offset > max_view_size) {
    end = (uint64_t)model_offset + max_view_offset;
  }

  return start <= row.row && row.row < end;
}

void HexTableModel::put_row_in_range(GlobalRow row) {
  if (row_is_in_range(row)) {
    return;
  }

  uint64_t new_model_offset;
  if ((uint64_t)row.row < max_view_size / 2) {
    new_model_offset = 0;
  } else if (row.row >= global_row_count() - max_view_size / 2) {
    new_model_offset = global_row_count() - max_view_size;
  } else {
    new_model_offset = row.row - max_view_size / 2;
  }

  beginResetModel();
  model_offset = new_model_offset;
  endResetModel();
}

void HexTableModel::handle_doubleclick(const QModelIndex &index) {
  auto row = index.row();
  auto col = index.column();
  auto offset = row_addr(global_row(row)) + col;
  auto node = ranges.get(offset);
  if (!node) {
    return;
  }
  node_info->change_root(node->node);
}

QPixmap HexTableModel::node_minimap(int len, QColor background_color) const {
  auto inner_len = len - 2;
  auto default_color = background_color.rgb();
  QImage image(1, len, QImage::Format_RGB32);
  image.setPixel(0, 0, default_color);
  image.setPixel(0, len - 1, default_color);
  for (size_t i = 0; i < inner_len; i++) {
    auto [min_row, max_row] = pixel_offset_global_row_range(i, inner_len);
    auto min_offset = row_addr(min_row);
    auto max_offset = row_addr(max_row) + columns - 1;
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
  header_size = metrics.size(0, "0x0000000000000000");
  header_size.setHeight(header_size.height() + 6);
  header_size.setWidth(header_size.width() + 10);
}
int HexTableModel::rowCount(const QModelIndex &parent) const {
  auto file_rows = global_row_count();
  return (int)std::min(file_rows, (size_t)max_view_size);
}
int HexTableModel::local_row(GlobalRow row) const {
  auto relative = (int64_t)row.row - (int64_t)model_offset;
  if (relative < 0) {
    return 0;
  }
  auto row_count = rowCount();
  if (relative >= row_count) {
    return row_count - 1;
  }
  return (int)relative;
}
GlobalRow HexTableModel::global_row(int row) const {
  return GlobalRow{row + model_offset};
}
