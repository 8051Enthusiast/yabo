#pragma once

#include <QFontDatabase>
#include <QJsonArray>
#include <QMainWindow>
#include <QRunnable>
#include <QTemporaryFile>
#include <QUrl>
#include <QtNetwork/QNetworkAccessManager>
#include <QtNetwork/QNetworkReply>

#include "filecontent.hpp"
#include "filerequester.hpp"

namespace Ui {
class YphbtWindow;
}

class ValTreeModel;
class HexTableModel;
class SelectionState;

class ExampleLoader : public QObject {
  Q_OBJECT

public:
  explicit ExampleLoader(QObject *parent = nullptr);
  ~ExampleLoader();

  void load_files(size_t index);
  void load_examples(const QUrl &url);

private slots:
  void handle_examples_loaded();
  void handle_source_finished();
  void handle_input_finished();
  void handle_network_error(QNetworkReply::NetworkError error);

signals:
  void examples_json_loaded(QJsonArray array) const;
  void files_loaded(QString source_content, QByteArray input_content) const;
  void error(QString error_message) const;

private:
  void start_download(const QUrl &url, bool is_source);

  QNetworkAccessManager *network_manager;
  QJsonArray json_data;

  struct DownloadState {
    QByteArray content;
    QUrl url;
    bool completed;
  };
  DownloadState input;
  DownloadState source;

  void handle_download_finished(QNetworkReply *reply, DownloadState &state);
};

class YphbtWindow : public QMainWindow {
  Q_OBJECT

public:
  YphbtWindow(QWidget *parent = nullptr, std::optional<QString> source = {},
              std::optional<QByteArray> input = {},
              std::optional<QUrl> examples = {});
  ~YphbtWindow();

private slots:
  void after_init();

  void on_actionCompile_triggered();
  void on_actionLoadFile_triggered();
  void on_actionBack_triggered();
  void on_actionForth_triggered();
  void on_actioncopyURL_triggered();

  void load_example(QString source, QByteArray input);
  void load_compiled_file(QString file_path);
  void compile_error(QString error);

  void on_tableView_doubleClicked(const QModelIndex &index);
  void on_treeView_doubleClicked(const QModelIndex &index);

private:
  void
  set_new_file_requester(std::unique_ptr<FileRequester> &&new_file_requester);

  void init_example_loader(QUrl &url);
  void init_examples(QJsonArray array);

  void set_font(const QFont &font);

  Ui::YphbtWindow *ui;
  std::unique_ptr<FileRequester> file_requester = nullptr;
  std::unique_ptr<ExampleLoader> loader = nullptr;
  std::shared_ptr<SelectionState> select = nullptr;
  FileRef file;
  std::optional<QUrl> compile_url;
  QFont current_font = QFontDatabase::systemFont(QFontDatabase::FixedFont);
};
