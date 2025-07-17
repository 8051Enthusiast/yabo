#include "parserview.hpp"
#include "compilewidget.hpp"
#include "filerequester.hpp"
#include "graph.hpp"
#include "hex.hpp"
#include "init.hpp"
#include "selectionstate.hpp"
#include "ui_parserview.h"
#include "valtreemodel.hpp"

#include <QHeaderView>
#include <QKeyEvent>
#include <QObject>
#include <QOpenGLWidget>
#include <filesystem>

ParserView::ParserView(QWidget *parent, std::unique_ptr<FileRequester> &&req)
    : QWidget(parent), ui(new Ui::ParserView), fileRequester(std::move(req)) {
  ui->setupUi(this);
  ui->graph_splitter->setSizes(QList<int>({INT_MAX / 1000, INT_MAX / 1000}));

  setup_models(*fileRequester);
  setup_graph_scene(*fileRequester);

  QOpenGLWidget *glWidget = new QOpenGLWidget();
  ui->graphicsView->setViewport(glWidget);

  connect(ui->compileWidget, &CompileWidget::compile_success, this,
          &ParserView::handle_compile_success);

  addAction(ui->actionCompile);

  start_threads();
}

ParserView::~ParserView() {
  if (graph_thread.isRunning()) {
    graph_thread.quit();
    graph_thread.wait();
  }
  delete ui;
}

void ParserView::on_treeView_doubleClicked(const QModelIndex &index) {
  treeModel->handle_doubleclick(index);
}

void ParserView::back() { select->undo(); }

void ParserView::forth() { select->redo(); }

void ParserView::keyPressEvent(QKeyEvent *event) {
  if ((event->modifiers() & Qt::ControlModifier) && event->key() == 'A' &&
      event->type() == QEvent::KeyPress) {
    ui->treeView->expandAll();
  }
}

void ParserView::on_tableView_doubleClicked(const QModelIndex &index) {
  hexModel->handle_doubleclick(index, select);
}

void ParserView::goto_address(size_t address) { select->jump_addr(address); }

void ParserView::request_parse(QString parser, size_t pos) {
  auto root = fileRequester->set_parser(parser, pos);
  select->set_root(root);
}

void ParserView::handle_compile_success(const QString &file_path) {
  auto new_file_requester = fileRequester->with_lib(file_path);

  auto error_message = new_file_requester->error_message();
  if (error_message != "") {
    ui->compileWidget->show_error(error_message);
    return;
  }

  std::filesystem::remove(file_path.toStdString());

  set_new_file_requester(std::move(new_file_requester));

  ui->compileWidget->clear_error();
}

void ParserView::on_actionCompile_triggered() {
  ui->compileWidget->trigger_compile();
}

void ParserView::set_new_file_requester(
    std::unique_ptr<FileRequester> &&new_file_requester) {
  graph_thread.quit();
  graph_thread.wait();

  setup_models(*new_file_requester);
  setup_graph_scene(*new_file_requester);
  fileRequester = std::move(new_file_requester);
  start_threads();
}

void ParserView::setup_models(FileRequester &new_file_requester) {
  select = std::make_shared<SelectionState>();

  auto newTreeModel = new ValTreeModel(&new_file_requester, select);
  ui->treeView->setModel(newTreeModel);
  treeModel.reset(newTreeModel);

  auto file = new_file_requester.file_ref();
  auto newHexModel = new HexTableModel(file, &new_file_requester);
  ui->tableView->set_model(newHexModel, select);
  hexModel.reset(newHexModel);

  init_hex(ui->tableView, hexModel.get(), &new_file_requester);
}

void ParserView::setup_graph_scene(FileRequester &new_file_requester) {
  auto graph = new Graph;
  graph->moveToThread(&graph_thread);
  scene =
      std::make_unique<GraphScene>(this, new_file_requester, *graph, select);

  connect(&graph_thread, &QThread::finished, graph, &QObject::deleteLater);
  connect(&new_file_requester, &FileRequester::update_graph, graph,
          &Graph::update_graph, Qt::QueuedConnection);

  ui->graphicsView->setScene(scene.get());
}

void ParserView::start_threads() {
  graph_thread.start();
  fileRequester->start_provider_thread();
}

void ParserView::set_source(const QString &source) {
  ui->compileWidget->set_source(source);
}
