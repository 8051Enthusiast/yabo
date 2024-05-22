#pragma once
#include "filecontent.hpp"
#include "node.hpp"
#include "rangemap.hpp"
#include <QAbstractItemDelegate>
#include <QAbstractTableModel>
#include <QColor>
#include <QPixmap>
#include <QStyledItemDelegate>

struct GlobalRow {
  size_t row;
};

class HexTableModel : public QAbstractTableModel {
  Q_OBJECT
public:
  HexTableModel(FileRef file, NodeInfoProvider *node_info);
  int rowCount(const QModelIndex &parent = QModelIndex()) const override;
  int columnCount(const QModelIndex &parent = QModelIndex()) const override {
    return columns;
  }
  QVariant data(const QModelIndex &index,
                int role = Qt::DisplayRole) const override;
  QVariant headerData(int section, Qt::Orientation orientation,
                      int role = Qt::DisplayRole) const override;

  void handle_doubleclick(const QModelIndex &index);

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

  QMenu *create_context_menu(const QModelIndex &index);
  size_t index_addr(const QModelIndex &index) const {
    return row_addr(global_row(index.row())) + index.column();
  }

  FileRef file;

signals:
  void updated_minimap();

public slots:
  void add_range(NodeRange range);

private:
  size_t global_row_count() const {
    return (file->span().size() + columns - 1) / columns;
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

class HexCell : public QStyledItemDelegate {
  Q_OBJECT
public:
  HexCell(QFont font, size_t file_size);
  void paint(QPainter *painter, const QStyleOptionViewItem &option,
             const QModelIndex &index) const override;
  QSize sizeHint(const QStyleOptionViewItem &option,
                 const QModelIndex &index) const override;
  QSize get_cell_size() const { return cell_size; }
  QSize get_header_size() const { return header_size; }
  void set_file_size(size_t file_size);
  void set_font(QFont font);

private:
  QSize padded(QSize size) const;
  QFont font;
  QSize cell_size;
  QSize header_size;
  size_t current_file_size;
};
