#pragma once

#include "node.hpp"
#include "request.hpp"

#include <QFont>
#include <QPoint>
#include <QStyledItemDelegate>
#include <QTableView>

class HexTableModel;
class SelectionState;

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

class HexTableView : public QTableView {
  Q_OBJECT
public:
  HexTableView(QWidget *parent = nullptr);
  void set_model(HexTableModel *model, std::shared_ptr<SelectionState> &select);
  void set_font(QFont font);
  void set_parser_requester(ParseRequester *parseRequester) {
    this->parseRequester = parseRequester;
  }
  void parse_menu(QPoint pos);

public slots:
  void goto_addr(size_t addr);
  void goto_node(Node node);
  void context_menu(const QPoint &pos);

private:
  void update_dimensions();
  HexTableModel *hexModel;
  ParseRequester *parseRequester = nullptr;
  std::unique_ptr<HexCell> hexCell;
  std::shared_ptr<SelectionState> select;
};
