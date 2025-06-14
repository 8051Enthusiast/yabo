#include "multifilehexview.hpp"
#include <QColor>
#include <QHeaderView>
#include <QMessageBox>
#include <algorithm>

MultiFileHexModel::MultiFileHexModel(QObject *parent)
    : QAbstractTableModel(parent) {}

int MultiFileHexModel::rowCount(const QModelIndex &) const {
  if (!update) {
    return 0;
  }
  return static_cast<int>(update->files->size());
}

int MultiFileHexModel::columnCount(const QModelIndex &) const {
  return static_cast<int>(after_columns + before_columns);
}

QVariant MultiFileHexModel::data(const QModelIndex &index, int role) const {
  if (!index.isValid() || !update ||
      index.row() >= static_cast<int>(update->files->size())) {
    return QVariant();
  }

  int row = index.row();
  int column = index.column();

  if (role == Qt::DisplayRole) {
    auto byte_val = get_byte_at(row, column);
    if (!byte_val) {
      return QVariant();
    }

    return QString("%1").arg(*byte_val, 2, 16, QChar('0'));
  } else if (role == Qt::BackgroundRole) {
    auto file_idx = get_file_index_for_row(row);

    if (!file_idx || !update->offsets.is_valid[*file_idx]) {
      return QColor(180, 180, 180);
    }

    if (!get_byte_offset_for_column(row, column)) {
      return QColor(180, 180, 180);
    }

    if (column < before_columns) {
      return QColor(200, 255, 200);
    } else {
      return QColor(255, 200, 200);
    }

  } else if (role == Qt::ForegroundRole) {
    return QColor(Qt::black);
  }

  return QVariant();
}

QVariant MultiFileHexModel::headerData(int section, Qt::Orientation orientation,
                                       int role) const {
  if (role == Qt::TextAlignmentRole) {
    return Qt::AlignCenter;
  } else if (role == Qt::BackgroundRole) {
    return QColor(Qt::lightGray);
  } else if (role != Qt::DisplayRole) {
    return QVariant();
  }
  if (orientation == Qt::Vertical) {
    if (auto file_idx = get_file_index_for_row(section)) {
      auto file_name = (*update->files)[*file_idx].file_name;

      if (!update->offsets.is_valid[*file_idx]) {
        return QString("[INVALID] %1").arg(file_name);
      } else {
        return file_name;
      }
    }

    return QVariant();
  } else {
    auto offset = section - before_columns;

    if (offset < 0) {
      return QString("-%1").arg(-offset, 2, 16, QChar('0'));
    } else {
      return QString("+%1").arg(offset, 2, 16, QChar('0'));
    }
  }
}

void MultiFileHexModel::update_files(std::shared_ptr<FileUpdate> update) {
  beginResetModel();

  this->update = update;

  before_columns = 15;
  after_columns = 16;
  for (size_t i = 0; i < update->files->size(); ++i) {
    if (!update->offsets.is_valid[i]) {
      continue;
    }

    const auto &file_info = (*update->files)[i];
    auto file_size = file_info.file_ref->total_size();
    auto parse_end = update->offsets.offsets[i];

    auto before = (int)std::min(parse_end, (size_t)INT_MAX / 2);
    before_columns = std::max(before_columns, before);
    auto after = (int)std::min(file_size - parse_end, (size_t)INT_MAX / 2);
    after_columns = std::max(after_columns, after);
  }

  endResetModel();
}

std::optional<size_t> MultiFileHexModel::get_file_index_for_row(int row) const {
  if (!update || row >= update->files->size() || row < 0) {
    return {};
  }

  return update->order.ordered_to_index[row];
}

std::optional<size_t>
MultiFileHexModel::get_byte_offset_for_column(int row, int column) const {
  auto file_idx = get_file_index_for_row(row);
  if (!file_idx) {
    return {};
  }

  auto shift = before_columns - update->offsets.offsets[*file_idx];
  if (column < shift) {
    return {};
  }
  auto offset = column - shift;
  auto size = update->files->at(*file_idx).file_ref->total_size();
  if (offset >= size) {
    return {};
  }
  return offset;
}

std::optional<uint8_t> MultiFileHexModel::get_byte_at(int row,
                                                      int column) const {
  auto file_idx = get_file_index_for_row(row);
  auto absolute_offset = get_byte_offset_for_column(row, column);
  if (!file_idx || !absolute_offset) {
    return {};
  }

  return update->files->at(*file_idx).file_ref->get_addr(*absolute_offset);
}

MultiFileHexView::MultiFileHexView(QWidget *parent) : QTableView(parent) {
  QFont hex_font("Monospace");
  hex_font.setStyleHint(QFont::TypeWriter);
  hex_font.setPointSize(12);

  hex_cell = std::make_unique<HexCell>(hex_font, 0);
  setItemDelegate(hex_cell.get());

  setAlternatingRowColors(true);
  setSelectionBehavior(QAbstractItemView::SelectItems);
  horizontalHeader()->setSectionResizeMode(QHeaderView::Fixed);
  verticalHeader()->setSectionResizeMode(QHeaderView::Fixed);

  setVerticalScrollBarPolicy(Qt::ScrollBarAsNeeded);
  setHorizontalScrollBarPolicy(Qt::ScrollBarAsNeeded);

  setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
  setVerticalScrollMode(QAbstractItemView::ScrollPerItem);

  update_column_widths();
}

void MultiFileHexView::set_model(MultiFileHexModel *new_model) {
  if (model) {
    disconnect(model, nullptr, this, nullptr);
  }

  model = new_model;
  QTableView::setModel(model);

  connect(new_model, &QAbstractItemModel::modelReset, this,
          &MultiFileHexView::handle_model_reset);
}

void MultiFileHexView::handle_model_reset() {
  if (!model) {
    return;
  }

  update_column_widths();

  auto parse_boundary_column = model->get_parsed_context_bytes();
  auto column_index = model->index(0, parse_boundary_column);
  scrollTo(column_index, QAbstractItemView::PositionAtCenter);

  resizeRowsToContents();
}

void MultiFileHexView::update_column_widths() {
  auto size = hex_cell->get_cell_size();
  auto vert_header = verticalHeader();
  vert_header->setSectionResizeMode(QHeaderView::Fixed);
  vert_header->setDefaultSectionSize(size.height());
  auto header_size = hex_cell->get_header_size();
  vert_header->setFixedWidth(header_size.width());
  auto horiz_header = horizontalHeader();
  horiz_header->setSectionResizeMode(QHeaderView::Fixed);
  horiz_header->setMinimumSectionSize(hex_cell->get_half_cell_size().width());
  int columns = model ? model->columnCount() : 16;
  for (int i = 0; i < columns; i++) {
    setColumnWidth(i, hex_cell->get_cell_size().width());
    setColumnWidth(columns + i, hex_cell->get_half_cell_size().width());
  }
}

std::optional<FileInfo>
MultiFileHexModel::get_file_info_for_index(const QModelIndex &index) const {
  if (!index.isValid()) {
    return {};
  }

  auto file_idx = get_file_index_for_row(index.row());
  if (!file_idx) {
    return {};
  }

  return (*update->files)[*file_idx];
}
