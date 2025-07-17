#include "compilewidget.hpp"
#include "compile.hpp"
#include "compiler_error_view.hpp"
#include "sourceeditor.hpp"
#include "ui_compilewidget.h"

CompileWidget::CompileWidget(QWidget *parent)
    : QWidget(parent), ui(new Ui::CompileWidget) {
  ui->setupUi(this);
  ui->errorView->hide();
}

CompileWidget::~CompileWidget() { delete ui; }

void CompileWidget::set_source(const QString &source) {
  ui->plainTextEdit->setPlainText(source);
}

QString CompileWidget::get_source() const {
  return ui->plainTextEdit->toPlainText();
}

void CompileWidget::set_compile_url(const std::optional<QUrl> &url) {
  compile_url = url;
}

void CompileWidget::set_font(const QFont &font) {
  ui->plainTextEdit->set_font(font);
  ui->errorView->setFont(font);
}

void CompileWidget::clear_error() {
  ui->errorView->clear();
  ui->errorView->hide();
}

void CompileWidget::show_error(const QString &error) {
  ui->errorView->set_error(error);
  ui->errorView->show();
}

void CompileWidget::trigger_compile() {
  auto program = ui->plainTextEdit->toPlainText();
  clear_error();

  if (compile_url) {
    start_remote_compile(*compile_url, program, this,
                         &CompileWidget::handle_compile_finished,
                         &CompileWidget::handle_compile_error);
  } else {
    start_local_compile(program, SourceKind::Content, this,
                        &CompileWidget::handle_compile_finished,
                        &CompileWidget::handle_compile_error);
  }
}

void CompileWidget::handle_compile_finished(QString file_path) {
  emit compile_success(file_path);
}

void CompileWidget::handle_compile_error(QString error) {
  show_error(error);
  emit compile_error(error);
}