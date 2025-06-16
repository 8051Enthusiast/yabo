#include "multifileview.hpp"
#include "compile.hpp"
#include "filerequester.hpp"
#include "flamegraph.hpp"
#include "multifilehexview.hpp"
#include "multifilemodel.hpp"
#include "parserview.hpp"
#include "sourceeditor.hpp"
#include "ui_multifileview.h"

MultiFileView::MultiFileView(std::unique_ptr<MultiFileParser> parser,
                             QWidget *parent)
    : QWidget(parent), ui(new Ui::MultiFileView), parser(std::move(parser)) {
  ui->setupUi(this);
  connect(this->parser.get(), &MultiFileParser::error, this,
          &MultiFileView::on_error);
  model = std::make_unique<MultiFileHexModel>(this);
  ui->tableView->set_model(model.get());
  ui->errorView->hide();
  connect(this->parser.get(), &MultiFileParser::files_updated, model.get(),
          &MultiFileHexModel::update_files);
  connect(this->parser.get(), &MultiFileParser::files_updated,
          ui->scrollAreaWidgetContents, &FlameGraph::get_update);
  connect(this->ui->scrollAreaWidgetContents, &FlameGraph::jump_to_pos,
          this->ui->tableView, &MultiFileHexView::jump_to_pos);
  connect(this, &MultiFileView::ascii_mode_changed,
          ui->scrollAreaWidgetContents, &FlameGraph::set_ascii);
  connect(this, &MultiFileView::ascii_mode_changed,
          model.get(), &MultiFileHexModel::set_ascii);
  addAction(ui->actionCompile);
}

MultiFileView::~MultiFileView() {
  delete ui;
  if (current_lib_name != "") {
    std::filesystem::remove(current_lib_name.toStdString());
  }
}

void MultiFileView::on_actionCompile_triggered() {
  auto program = ui->plainTextEdit->toPlainText();
  ui->errorView->clear();
  ui->errorView->hide();

  start_local_compile(program, SourceKind::Content, this,
                      &MultiFileView::load_compiled_file,
                      &MultiFileView::on_error);
}

void MultiFileView::on_error(QString error) {
  ui->errorView->show();
  ui->errorView->set_error(error);
}

void MultiFileView::load_compiled_file(QString filePath) {
  this->parser->new_library(filePath);
  if (!current_lib_name.isEmpty()) {
    std::filesystem::remove(current_lib_name.toStdString());
  }
  current_lib_name = filePath;
}

void MultiFileView::on_tableView_doubleClicked(const QModelIndex &index) {
  auto file_info = model->get_file_info_for_index(index);
  if (!file_info || current_lib_name.isEmpty()) {
    return;
  }

  auto file_ref = file_info->file_ref;

  auto file_requester = FileRequesterFactory{}.create_file_requester(
      current_lib_name, file_ref, false);
  if (file_requester->error_message() != "") {
    ui->errorView->set_error(file_requester->error_message());
    ui->errorView->show();
    return;
  }

  auto view = new ParserView(nullptr, std::move(file_requester));
  view->request_parse("main", 0);
  emit new_tab_requested(view, file_info->file_name);
}

void MultiFileView::keyPressEvent(QKeyEvent *event) {
  if (event->key() == Qt::Key_Control) {
    emit ascii_mode_changed(true);
  }
  QWidget::keyPressEvent(event);
}

void MultiFileView::keyReleaseEvent(QKeyEvent *event) {
  if (event->key() == Qt::Key_Control) {
    emit ascii_mode_changed(false);
  }
  QWidget::keyReleaseEvent(event);
}
