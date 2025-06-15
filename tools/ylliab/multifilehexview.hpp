#pragma once

#include "hexview.hpp"
#include "multifilemodel.hpp"
#include <QAbstractTableModel>
#include <QTableView>
#include <QWidget>
#include <memory>
#include <qabstractitemmodel.h>

class MultiFileHexModel : public QAbstractTableModel {
  Q_OBJECT

public:
  explicit MultiFileHexModel(QObject *parent = nullptr);

  int rowCount(const QModelIndex &parent = QModelIndex()) const override;
  int columnCount(const QModelIndex &parent = QModelIndex()) const override;
  QVariant data(const QModelIndex &index,
                int role = Qt::DisplayRole) const override;
  QVariant headerData(int section, Qt::Orientation orientation,
                      int role = Qt::DisplayRole) const override;

  int get_parsed_context_bytes() const { return before_columns; }
  std::optional<FileInfo>
  get_file_info_for_index(const QModelIndex &index) const;

public slots:
  void update_files(std::shared_ptr<FileUpdate> update);

private:
  std::shared_ptr<FileUpdate> update;
  int after_columns = 0;
  int before_columns = 0;

  std::optional<size_t> get_file_index_for_row(int row) const;
  std::optional<size_t> get_byte_offset_for_column(int row, int column) const;
  std::optional<uint8_t> get_byte_at(int row, int column) const;
};

class MultiFileHexView : public QTableView {
  Q_OBJECT

public:
  explicit MultiFileHexView(QWidget *parent = nullptr);

  void set_model(MultiFileHexModel *model);
public slots:
  void jump_to_pos(size_t row);

private slots:
  void handle_model_reset();

private:
  void update_column_widths();

  MultiFileHexModel *model = nullptr;
  std::unique_ptr<HexCell> hex_cell;
};