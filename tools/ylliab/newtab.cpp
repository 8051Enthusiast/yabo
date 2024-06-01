#include <QFile>
#include <QFileDialog>
#include <QFontDatabase>
#include <QMessageBox>
#include <filesystem>

#include "./ui_newtab.h"
#include "compile.hpp"
#include "filerequester.hpp"
#include "newtab.hpp"

NewTab::NewTab(QWidget *parent) : QDialog(parent), ui(new Ui::NewTab) {
  ui->setupUi(this);
  auto font = QFontDatabase::systemFont(QFontDatabase::FixedFont);
  font.setPointSize(12);
  ui->errors->setFont(font);
}

NewTab::~NewTab() { delete ui; }

void NewTab::on_intputFileButton_clicked() {
  auto fileName = QFileDialog::getOpenFileName(this, "Open Input File...");
  if (fileName != "") {
    ui->inputFileEdit->setText(fileName);
  }
}

void NewTab::on_parserFileButton_clicked() {
  auto fileName = QFileDialog::getOpenFileName(this, "Open Parser File...");
  if (fileName != "") {
    ui->parserFileEdit->setText(fileName);
  }
}

void NewTab::show_error(QString text) {
  ui->errors->set_error(text);
  setEnabled(true);
}

void NewTab::done(int r) {
  if (r != QDialog::Accepted) {
    return QDialog::done(r);
  }
  if (ui->parserFileEdit->text() == "") {
    show_error("Parser file empty!");
    return;
  }
  if (ui->inputFileEdit->text() == "") {
    show_error("Input file empty!");
    return;
  }
  auto program_path = ui->parserFileEdit->text();
  auto file = QFile(program_path);
  if (!file.open(QIODevice::ReadOnly)) {
    show_error("Failed to open parser source");
    return;
  }
  auto program = file.readAll();
  start_local_compile(program, this, &NewTab::load_compiled_file,
                      &NewTab::show_error);
  setEnabled(false);
}

QString NewTab::get_input_file() { return ui->inputFileEdit->text(); }

void NewTab::load_compiled_file(QString file_path) {
  auto factory = FileRequesterFactory();
  auto new_file_requester = factory.create_file_requester(
      file_path, ui->inputFileEdit->text(), ui->fetchEager->checkState());
  std::filesystem::remove(file_path.toStdString());
  auto error = new_file_requester->error_message();
  if (error != "") {
    show_error(error);
    return;
  }
  file_requester = std::move(new_file_requester);
  setEnabled(true);
  ui->errors->clear();
  QDialog::done(QDialog::Accepted);
}
