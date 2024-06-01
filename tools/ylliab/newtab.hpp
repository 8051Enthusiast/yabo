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

private slots:
  void on_intputFileButton_clicked();
  void on_parserFileButton_clicked();
  void load_compiled_file(QString file_path);
  void show_error(QString text);

protected:
  void done(int r) override;

private:
  Ui::NewTab *ui;
  std::unique_ptr<FileRequester> file_requester = nullptr;
};
