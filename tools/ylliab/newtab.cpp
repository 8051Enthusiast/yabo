#include <QFileDialog>
#include <QMessageBox>

#include "./ui_newtab.h"
#include "filerequester.hpp"
#include "newtab.hpp"

NewTab::NewTab(QWidget *parent) : QDialog(parent), ui(new Ui::NewTab) {
  ui->setupUi(this);
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

void NewTab::done(int r) {
  if (r != QDialog::Accepted) {
    return QDialog::done(r);
  }
  auto on_fail = [](QString text) {
    QMessageBox messageBox;
    messageBox.setText(text);
    messageBox.setStandardButtons(QMessageBox::Close);
    return messageBox.exec();
  };
  if (ui->parserFileEdit->text() == "") {
    on_fail("Parser file empty!");
    return;
  }
  if (ui->inputFileEdit->text() == "") {
    on_fail("Input file empty!");
    return;
  }
  if (ui->parserNameEdit->text() == "") {
    on_fail("Parser name empty!");
    return;
  }
  auto factory = FileRequesterFactory();
  auto new_file_requester = factory.create_file_requester(
      ui->parserFileEdit->text(), ui->inputFileEdit->text(),
      ui->parserNameEdit->text(), ui->fetchEager->checkState());
  auto error = new_file_requester->error_message();
  if (error != "") {
    on_fail(error);
    return;
  }
  file_requester = std::move(new_file_requester);
  QDialog::done(r);
}

QString NewTab::get_input_file() { return ui->inputFileEdit->text(); }
