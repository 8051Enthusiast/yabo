#include "hex.hpp"
#include <QPainter>
#include <qnamespace.h>

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
