#include "parserview.hpp"
#include "graph.hpp"
#include "hex.hpp"
#include "init.hpp"
#include "selectionstate.hpp"
#include "ui_parserview.h"
#include "yabotreemodel.hpp"

#include <QHeaderView>
#include <QKeyEvent>
#include <QObject>
#include <QOpenGLWidget>

#include <iostream>

ParserView::ParserView(QWidget *parent, std::unique_ptr<FileRequester> &&req)
    : QWidget(parent), ui(new Ui::ParserView), fileRequester(std::move(req)) {
  ui->setupUi(this);
  ui->graph_splitter->setSizes(QList<int>({INT_MAX / 1000, INT_MAX / 1000}));
  select = std::make_shared<SelectionState>();
  treeModel = std::make_unique<YaboTreeModel>(fileRequester.get(), select);
  ui->treeView->setModel(treeModel.get());

  auto file = fileRequester->file_ref();
  hexModel = std::make_unique<HexTableModel>(file, fileRequester.get());
  ui->tableView->set_model(hexModel.get(), select);

  auto graph = new Graph;
  graph->moveToThread(&graph_thread);
  scene = std::make_unique<GraphScene>(this, *fileRequester, *graph, select);

  connect(&graph_thread, &QThread::finished, graph, &QObject::deleteLater);
  connect(fileRequester.get(), &FileRequester::update_graph, graph,
          &Graph::update_graph, Qt::QueuedConnection);
  init_hex_and_tree(ui->tableView, ui->treeView, hexModel.get(),
                    treeModel.get(), fileRequester.get(), select);

  ui->graphicsView->setScene(scene.get());

  QOpenGLWidget *glWidget = new QOpenGLWidget();
  ui->graphicsView->setViewport(glWidget);

  graph_thread.start();
  fileRequester->start_executor_thread();
}

ParserView::~ParserView() {
  graph_thread.quit();
  graph_thread.wait();
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
