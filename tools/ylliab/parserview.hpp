#pragma once
#include "filerequester.hpp"
#include "graph.hpp"
#include "hex.hpp"

#include <QWidget>

namespace Ui {
class ParserView;
}

class ParserView : public QWidget {
  Q_OBJECT

public:
  explicit ParserView(QWidget *parent, std::unique_ptr<FileRequester> &&req);
  ~ParserView();
  void setParserName(QString fileRequester);
  void keyPressEvent(QKeyEvent *event) override;

private slots:
  void on_lineEdit_returnPressed();

  void on_treeView_doubleClicked(const QModelIndex &index);

  void on_undoButton_clicked();

  void on_redoButton_clicked();

private:
  Ui::ParserView *ui;
  std::unique_ptr<FileRequester> fileRequester;
  std::unique_ptr<GraphScene> scene;
  std::unique_ptr<YaboTreeModel> treeModel;
  std::unique_ptr<HexTableModel> hexModel;
  std::unique_ptr<HexCell> hexCell;
  QThread graph_thread;
};
