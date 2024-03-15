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
  YphbtWindow(QWidget *parent = nullptr, std::optional<QString> source = {},
              std::optional<QByteArray> input = {});
  ~YphbtWindow();

private slots:
  void after_init();

  void on_actionCompile_triggered();
  void on_actionLoadFile_triggered();
  void on_actionBack_triggered();
  void on_actionForth_triggered();
  void on_actionEnsizen_triggered();
  void on_actionDesizen_triggered();
  void reset_font_size();

  void load_compiled_file(QString file_path);
  void compile_error(QString error);

  void on_tableView_doubleClicked(const QModelIndex &index);
  void on_treeView_doubleClicked(const QModelIndex &index);

  private:
  void
  set_new_file_requester(std::unique_ptr<FileRequester> &&new_file_requester);

  void set_font(const QFont &font);

  Ui::YphbtWindow *ui;
  std::unique_ptr<FileRequester> file_requester;
  std::unique_ptr<YaboTreeModel> treeModel;
  std::unique_ptr<HexTableModel> hexModel;
  FileRef file;
  std::optional<QUrl> compile_url;
  static constexpr int default_font_size = 12;
  static constexpr int minimum_font_size = 4;
  QFont current_font = QFont("Monospace", default_font_size);
};
