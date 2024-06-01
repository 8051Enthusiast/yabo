#include "ylliabwindow.hpp"
#include "./ui_ylliabwindow.h"
#include "filerequester.hpp"
#include "newtab.hpp"
#include "parserview.hpp"

#include <QFileDialog>
#include <QMessageBox>

YlliabWindow::YlliabWindow(QWidget *parent)
    : QMainWindow(parent), ui(new Ui::YlliabWindow) {
  ui->setupUi(this);
  new_tab = new NewTab(this);
}

YlliabWindow::~YlliabWindow() { delete ui; }
void YlliabWindow::on_actionNewTab_triggered() {
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

void YlliabWindow::on_tabWidget_tabCloseRequested(int index) {
  auto view = ui->tabWidget->widget(index);
  ui->tabWidget->removeTab(index);
  delete view;
}

void YlliabWindow::on_actionBack_triggered() {
  auto widget = current_parser_view();
  if (!widget)
    return;
  widget->back();
}

void YlliabWindow::on_actionForth_triggered() {
  auto widget = current_parser_view();
  if (!widget)
    return;
  widget->forth();
}

ParserView *YlliabWindow::current_parser_view() const {
  return dynamic_cast<ParserView *>(ui->tabWidget->currentWidget());
}
