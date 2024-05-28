#include "mainwindow.hpp"
#include "./ui_mainwindow.h"
#include "filerequester.hpp"
#include "newtab.hpp"
#include "parserview.hpp"

#include <QFileDialog>
#include <QMessageBox>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent), ui(new Ui::MainWindow) {
  ui->setupUi(this);
  new_tab = new NewTab(this);
}

MainWindow::~MainWindow() { delete ui; }
void MainWindow::on_actionNewTab_triggered() {
  new_tab->exec();
  if (new_tab->result() != QDialog::Accepted) {
    return;
  }
  auto fileRequester = new_tab->get_file_requester();
  auto view = new ParserView(nullptr, std::move(fileRequester));
  auto name = new_tab->get_input_file();
  ui->tabWidget->addTab(view, name);
  auto newTabIndex = ui->tabWidget->count() - 1;
  ui->tabWidget->setCurrentIndex(newTabIndex);
}

void MainWindow::on_tabWidget_tabCloseRequested(int index) {
  auto view = ui->tabWidget->widget(index);
  ui->tabWidget->removeTab(index);
  delete view;
}

void MainWindow::on_actionBack_triggered() {
  auto widget = current_parser_view();
  if (!widget)
    return;
  widget->back();
}

void MainWindow::on_actionForth_triggered() {
  auto widget = current_parser_view();
  if (!widget)
    return;
  widget->forth();
}

ParserView *MainWindow::current_parser_view() const {
  return static_cast<ParserView *>(ui->tabWidget->currentWidget());
}
