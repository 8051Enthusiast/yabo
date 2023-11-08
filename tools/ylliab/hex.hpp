#pragma once
#include "filecontent.hpp"
#include "node.hpp"
#include "rangemap.hpp"
#include <QAbstractItemDelegate>
#include <QAbstractTableModel>
#include <QColor>

class HexTableModel : public QAbstractTableModel {
  Q_OBJECT
public:
  HexTableModel(FileRef file, NodeInfoProvider *node_info)
      : file(file), node_info(node_info) {}
  int rowCount(const QModelIndex &parent = QModelIndex()) const override {
    return (file->span().size() + (columns - 1)) / columns;
  }
  int columnCount(const QModelIndex &parent = QModelIndex()) const override {
    return columns;
  }
  QVariant data(const QModelIndex &index,
                int role = Qt::DisplayRole) const override;
  QVariant headerData(int section, Qt::Orientation orientation,
                      int role = Qt::DisplayRole) const override;
  
  void handle_doubleclick(const QModelIndex &index);

public slots:
  void add_range(NodeRange range);

private:
  FileRef file;
  RangeMap ranges;
  int columns = 16;
  NodeInfoProvider *node_info;
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

private:
  QFont font;
  QSize cell_size;
  constexpr static int corner_size = 6;
  constexpr static int margin = 1;
};