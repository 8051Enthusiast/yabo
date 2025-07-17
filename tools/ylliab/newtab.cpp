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
  reset_ui_state();
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
    reset_ui_state();
    return QDialog::done(r);
  }
  if (ui->parserFileEdit->text() == "") {
    show_error("Parser file empty!");
    return;
  }
  if (!preset_file && ui->inputFileEdit->text() == "") {
    show_error("Input file empty!");
    return;
  }
  auto program_path = ui->parserFileEdit->text();
  start_local_compile(program_path, SourceKind::Path, this,
                      &NewTab::load_compiled_file, &NewTab::show_error);
  setEnabled(false);
}

QString NewTab::get_input_file() {
  if (ui->inputFileEdit->text().isEmpty()) {
    return "new tab";
  }
  return ui->inputFileEdit->text();
}

QString NewTab::get_parser_path() {
  return ui->parserFileEdit->text();
}

void NewTab::load_compiled_file(QString file_path) {
  auto factory = FileRequesterFactory();
  std::unique_ptr<FileRequester> new_file_requester;
  if (preset_file) {
    new_file_requester = factory.create_file_requester(
        file_path, *preset_file, ui->fetchEager->checkState());
  } else {
    new_file_requester = factory.create_file_requester(
        file_path, ui->inputFileEdit->text(), ui->fetchEager->checkState());
  }
  std::filesystem::remove(file_path.toStdString());
  auto error = new_file_requester->error_message();
  if (error != "") {
    show_error(error);
    return;
  }
  file_requester = std::move(new_file_requester);
  setEnabled(true);
  reset_ui_state();
  QDialog::done(QDialog::Accepted);
}

void NewTab::reset_ui_state() {
  ui->inputFile->show();
  ui->parserFile->show();
  ui->errors->clear();
  preset_file = {};
}

void NewTab::set_preset_file_path(QString p) { ui->inputFileEdit->setText(p); }

void NewTab::set_preset_file(FileRef f) {
  ui->inputFile->hide();
  preset_file = f;
}

void NewTab::set_preset_parser_path(QString p) {
  ui->parserFileEdit->setText(p);
}
