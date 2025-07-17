#pragma once
#include "filerequester.hpp"
#include <QDialog>
#include <memory>

class FileRequester;

namespace Ui {
class NewTab;
}

class NewTab : public QDialog {
  Q_OBJECT

public:
  explicit NewTab(QWidget *parent = nullptr);
  ~NewTab();

  std::unique_ptr<FileRequester> get_file_requester() {
    return std::move(file_requester);
  }
  QString get_input_file();
  QString get_parser_path();

  void set_preset_file_path(QString p);
  void set_preset_file(FileRef f);
  void set_preset_parser_path(QString p);
  void set_error(QString err);

public slots:
  void show_error(QString text);
private slots:
  void on_intputFileButton_clicked();
  void on_parserFileButton_clicked();
  void load_compiled_file(QString file_path);

protected:
  void done(int r) override;

private:
  void reset_ui_state();
  std::optional<FileRef> preset_file = {};
  Ui::NewTab *ui;
  std::unique_ptr<FileRequester> file_requester = nullptr;
};
