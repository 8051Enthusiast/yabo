#include "hex.hpp"
#include "selectionstate.hpp"
#include <QImage>
#include <QMenu>
#include <QPainter>
#include <QPixmap>
#include <qitemselectionmodel.h>
#include <qnamespace.h>
#include <vector>

HexTableModel::HexTableModel(FileRef file, NodeInfoProvider *node_info,
                             QObject *parent)
    : QAbstractTableModel(parent), file(file), node_info(node_info) {
  file_address_digit_count = address_digit_count(file->end_address());
}

static char char_repr(uint8_t byte) {
  if (byte < 0x20 || byte >= 0x7f) {
    return '.';
  }
  return byte;
}

QVariant HexTableModel::data(const QModelIndex &index, int role) const {
  auto offset = index_addr(index);
  bool is_ascii = is_ascii_column(index);
  if (role == Qt::DisplayRole) {
    if (offset >= file->end_address()) {
      return QVariant();
    }
    if (is_ascii) {
      return QString("%1").arg(char_repr(file->get_addr(offset)));
    } else {
      return QString("%1").arg(file->get_addr(offset), 2, 16, QChar('0'));
    }
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
    return QString("%1").arg(row_addr(global_row(section)),
                             file_address_digit_count, 16, QChar('0'));
  }
  if (role == Qt::DisplayRole && orientation == Qt::Horizontal) {
    if (section >= columns) {
      return QString("%1").arg(section - columns, 1, 16, QChar('0'));
    } else {
      return QString("%1").arg(section, 2, 16, QChar('0'));
    }
  }
  return QVariant();
}

void HexTableModel::add_range(NodeRange range) {
  ranges.insert(range);
  auto start_row = local_row(addr_row(range.start));
  auto end_row = local_row(addr_row(range.end - 1));
  emit dataChanged(createIndex(start_row, 0),
                   createIndex(end_row, columnCount() - 1));
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
  return GlobalRow{address / bytes_per_row()};
}

size_t HexTableModel::row_addr(GlobalRow row) const {
  return row.row * bytes_per_row();
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

void HexTableModel::handle_doubleclick(
    const QModelIndex &index, std::shared_ptr<SelectionState> &select) {
  auto offset = index_addr(index);
  auto node = ranges.get(offset);
  if (!node) {
    return;
  }
  select->set_root(node->node);
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
    auto max_offset = row_addr(max_row) + bytes_per_row() - 1;
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
std::vector<NodeRange> const &
HexTableModel::nodes_at(const QModelIndex &index) const {
  auto offset = index_addr(index);
  return ranges.get_all(offset);
}

QMenu *
HexTableModel::create_context_menu(const QModelIndex &index,
                                   std::shared_ptr<SelectionState> &select) {
  auto addr = index_addr(index);
  auto &nodes = ranges.get_all(addr);
  if (nodes.empty()) {
    return nullptr;
  }
  auto menu = new QMenu();
  for (auto &node : nodes) {
    auto action = new QAction(menu);
    auto color = node_info->node_color(node.node);
    auto pixmap = QPixmap(32, 32);
    pixmap.fill(color);
    action->setText(node_info->node_name(node.node));
    action->setIcon(QIcon(pixmap));
    action->connect(action, &QAction::triggered,
                    [select, node]() { select->set_root(node.node); });
    menu->addAction(action);
  }
  return menu;
}

HexSelectionModel::HexSelectionModel(
    HexTableModel *model, std::shared_ptr<SelectionState> selection_state)
    : QItemSelectionModel(model, model), model(model),
      selection_state(selection_state) {
  connect(selection_state.get(), &SelectionState::selection_changed, this,
          &HexSelectionModel::set_selection);
  set_selection(selection_state->get_selection());
}

void HexSelectionModel::set_selection(std::optional<TreeIndex> idx) {
  if (!idx) {
    clearSelection();
    return;
  }
  auto addresses = model->get_node_info()->idx_range(*idx);
  if (!addresses) {
    clearSelection();
    return;
  }
  auto [start, end] = *addresses;
  if (start == end) {
    clearSelection();
    return;
  }

  auto start_row = model->addr_row(start);
  auto last_row = model->addr_row(end - 1);
  auto start_local_row = model->local_row(start_row);
  auto last_local_row = model->local_row(last_row);
  auto col = model->bytes_per_row();
  auto start_column = start % col;
  auto last_column = (end - 1) % col;
  auto ascii_start_column = start_column + col;
  auto ascii_last_column = last_column + col;
  auto ascii_col = col + col;
  auto selection = QItemSelection();
  if (start_local_row == last_local_row) {
    selection.select(model->index(start_local_row, start_column),
                     model->index(start_local_row, last_column));

    selection.select(model->index(start_local_row, ascii_start_column),
                     model->index(start_local_row, ascii_last_column));
  } else {
    selection.select(model->index(start_local_row, start_column),
                     model->index(start_local_row, col - 1));

    selection.select(model->index(start_local_row, ascii_start_column),
                     model->index(start_local_row, ascii_col - 1));
    if (last_local_row - start_local_row >= 2) {
      // select both hex and ascii at once
      selection.select(model->index(start_local_row + 1, 0),
                       model->index(last_local_row - 1, ascii_col - 1));
    }

    selection.select(model->index(last_local_row, 0),
                     model->index(last_local_row, last_column));

    selection.select(model->index(last_local_row, col),
                     model->index(last_local_row, ascii_last_column));
  }
  select(selection, QItemSelectionModel::ClearAndSelect);
}
