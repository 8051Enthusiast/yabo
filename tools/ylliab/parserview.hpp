#pragma once

#include <QThread>
#include <QWidget>

class HexTableModel;
class FileRequester;
class GraphScene;
class ValTreeModel;
class SelectionState;
class CompileWidget;

namespace Ui {
class ParserView;
}

class ParserView : public QWidget {
  Q_OBJECT

public:
  explicit ParserView(QWidget *parent, std::unique_ptr<FileRequester> &&req);
  ~ParserView();
  void request_parse(QString fileRequester, size_t pos);
  void keyPressEvent(QKeyEvent *event) override;
  void goto_address(size_t address);
  void back();
  void forth();
  void set_source(const QString &source);

private slots:
  void on_treeView_doubleClicked(const QModelIndex &index);
  void on_tableView_doubleClicked(const QModelIndex &index);
  void handle_compile_success(const QString &file_path);
  void on_actionCompile_triggered();

private:
  void set_new_file_requester(std::unique_ptr<FileRequester> &&new_file_requester);
  void setup_models(FileRequester &new_file_requester);
  void setup_graph_scene(FileRequester &new_file_requester);
  void start_threads();
  Ui::ParserView *ui;
  std::unique_ptr<FileRequester> fileRequester;
  std::unique_ptr<GraphScene> scene;
  std::unique_ptr<ValTreeModel> treeModel;
  std::unique_ptr<HexTableModel> hexModel;
  std::shared_ptr<SelectionState> select;
  QThread graph_thread;
};
