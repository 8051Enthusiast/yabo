#pragma once
#include "filecontent.hpp"
#include "node.hpp"
#include "rangemap.hpp"
#include <QAbstractItemDelegate>
#include <QAbstractTableModel>
#include <QColor>
#include <QItemSelectionModel>
#include <QPixmap>
#include <memory>

class SelectionState;

struct GlobalRow {
  size_t row;
};

static inline uint8_t address_digit_count(uint64_t file_size) {
  if (!file_size) {
    return 4;
  }
  if ((file_size - 1) >> 32) {
    return 16;
  }
  if ((file_size - 1) >> 16) {
    return 8;
  }
  return 4;
}

class HexTableModel : public QAbstractTableModel {
  Q_OBJECT
public:
  HexTableModel(FileRef file, NodeInfoProvider *node_info,
                QObject *parent = nullptr);
  int rowCount(const QModelIndex &parent = QModelIndex()) const override;
  int columnCount(const QModelIndex &parent = QModelIndex()) const override {
    return columns;
  }
  QVariant data(const QModelIndex &index,
                int role = Qt::DisplayRole) const override;
  QVariant headerData(int section, Qt::Orientation orientation,
                      int role = Qt::DisplayRole) const override;

  void handle_doubleclick(const QModelIndex &index,
                          std::shared_ptr<SelectionState> &select);

  QPixmap node_minimap(int len, QColor default_color) const;

  std::optional<size_t> node_addr(Node node) const;

  int local_row(GlobalRow row) const;
  GlobalRow global_row(int row) const;

  GlobalRow addr_row(size_t address) const;
  size_t row_addr(GlobalRow row) const;

  std::pair<GlobalRow, GlobalRow> pixel_offset_global_row_range(int offset,
                                                                int max) const;
  int row_pixel_offset(GlobalRow row, int max) const;

  bool row_is_in_range(GlobalRow addr) const;
  void put_row_in_range(GlobalRow addr);

  QMenu *create_context_menu(const QModelIndex &index,
                             std::shared_ptr<SelectionState> &select);
  size_t index_addr(const QModelIndex &index) const {
    return row_addr(global_row(index.row())) + index.column();
  }
  NodeInfoProvider *get_node_info() const noexcept { return node_info; }
  FileRef file;

signals:
  void updated_minimap();

public slots:
  void add_range(NodeRange range);

private:
  size_t global_row_count() const {
    return (file->end_address() + columns - 1) / columns;
  }
  std::vector<NodeRange> const &nodes_at(QModelIndex &index) const;
  RangeMap ranges;
  int columns = 16;
  int file_address_digit_count;
  size_t model_offset = 0;
  NodeInfoProvider *node_info;

  static constexpr uint64_t max_view_size = (uint64_t)1 << 26;
  static constexpr uint64_t min_view_offset = max_view_size / 4;
  static constexpr uint64_t max_view_offset = 3 * max_view_size / 4;
};

class HexSelectionModel : public QItemSelectionModel {
  Q_OBJECT
public:
  HexSelectionModel(HexTableModel *model,
                    std::shared_ptr<SelectionState> selection_state);

public slots:
  void set_selection(std::optional<TreeIndex> idx);

private:
  HexTableModel *model;
  std::shared_ptr<SelectionState> selection_state;
};