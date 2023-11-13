#pragma once
#include "filecontent.hpp"
#include "node.hpp"
#include "rangemap.hpp"
#include <QAbstractItemDelegate>
#include <QAbstractTableModel>
#include <QColor>
#include <QPixmap>

struct GlobalRow {
  size_t row;
};

class HexTableModel : public QAbstractTableModel {
  Q_OBJECT
public:
  HexTableModel(FileRef file, NodeInfoProvider *node_info)
      : file(file), node_info(node_info) {}
  int rowCount(const QModelIndex &parent = QModelIndex()) const override;
  int columnCount(const QModelIndex &parent = QModelIndex()) const override {
    return columns;
  }
  QVariant data(const QModelIndex &index,
                int role = Qt::DisplayRole) const override;
  QVariant headerData(int section, Qt::Orientation orientation,
                      int role = Qt::DisplayRole) const override;

  void handle_doubleclick(const QModelIndex &index);

  QPixmap node_minimap(int len) const;

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

signals:
  void updated_minimap();

public slots:
  void add_range(NodeRange range);

private:
  size_t global_row_count() const {
    return (file->span().size() + columns - 1) / columns;
  }
  FileRef file;
  RangeMap ranges;
  int columns = 16;
  size_t model_offset = 0;
  NodeInfoProvider *node_info;

  static constexpr uint64_t max_view_size = (uint64_t)1 << 26;
  static constexpr uint64_t min_view_offset = max_view_size / 4;
  static constexpr uint64_t max_view_offset = 3 * max_view_size / 4;
};

class HexCell : public QAbstractItemDelegate {
  Q_OBJECT
public:
  HexCell(QFont font);
  void paint(QPainter *painter, const QStyleOptionViewItem &option,
             const QModelIndex &index) const override;
  QSize sizeHint(const QStyleOptionViewItem &option,
                 const QModelIndex &index) const override;
  QSize get_cell_size() const { return cell_size; }
  QSize get_header_size() const { return header_size; }

private:
  QFont font;
  QSize cell_size;
  QSize header_size;
  constexpr static int corner_size = 6;
  constexpr static int margin = 1;
};
