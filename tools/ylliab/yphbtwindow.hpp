#pragma once

#include "filerequester.hpp"
#include "hex.hpp"
#include "yabotreemodel.hpp"
#include <QMainWindow>
#include <QRunnable>
#include <QTemporaryFile>
#include <QUrl>

namespace Ui {
class YphbtWindow;
}

class YphbtWindow : public QMainWindow {
  Q_OBJECT

public:
  explicit YphbtWindow(QWidget *parent = nullptr);
  ~YphbtWindow();

private slots:
  void on_actionCompile_triggered();
  void on_actionLoadFile_triggered();
  void load_compiled_file(QString file_path);
  void compile_error(QString error);

  void on_tableView_doubleClicked(const QModelIndex &index);
  void on_treeView_doubleClicked(const QModelIndex &index);


  private:
  void
  set_new_file_requester(std::unique_ptr<FileRequester> &&new_file_requester);

  Ui::YphbtWindow *ui;
  std::unique_ptr<FileRequester> file_requester;
  std::unique_ptr<YaboTreeModel> treeModel;
  std::unique_ptr<HexTableModel> hexModel;
  FileRef file;
};
