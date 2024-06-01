#pragma once

#include <QWidget>
#include <QThread>

class HexTableModel;
class FileRequester;
class GraphScene;
class ValTreeModel;
class SelectionState;

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
  void back();
  void forth();

private slots:
  void on_treeView_doubleClicked(const QModelIndex &index);
  void on_tableView_doubleClicked(const QModelIndex &index);

  private:
  Ui::ParserView *ui;
  std::unique_ptr<FileRequester> fileRequester;
  std::unique_ptr<GraphScene> scene;
  std::unique_ptr<ValTreeModel> treeModel;
  std::unique_ptr<HexTableModel> hexModel;
  std::shared_ptr<SelectionState> select;
  QThread graph_thread;
};
