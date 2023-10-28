#include "parserview.hpp"
#include "ui_parserview.h"
#include "yabotreemodel.hpp"

#include <QObject>
#include <QOpenGLWidget>
#include <QKeyEvent>

#include <iostream>

ParserView::ParserView(QWidget *parent, std::unique_ptr<FileRequester> &&req)
    : QWidget(parent), ui(new Ui::ParserView), fileRequester(std::move(req)),
      treeModel(fileRequester->get_tree_model()) {
  ui->setupUi(this);
  ui->treeView->setModel(&treeModel);
  auto graph = new Graph(Node{treeModel.get_root()});
  graph->moveToThread(&graph_thread);
  scene = std::make_unique<GraphScene>(this, *fileRequester, *graph);
  connect(&graph_thread, &QThread::finished, graph, &QObject::deleteLater);
  connect(fileRequester.get(), &FileRequester::update_graph, graph,
          &Graph::update_graph, Qt::QueuedConnection);
  connect(scene.get(), &GraphScene::node_double_clicked, fileRequester.get(),
          &FileRequester::change_root);
  connect(fileRequester.get(), &FileRequester::root_changed, scene.get(),
          &GraphScene::select_node);
  ui->graphicsView->setScene(scene.get());
  QOpenGLWidget *glWidget = new QOpenGLWidget();
  ui->graphicsView->setViewport(glWidget);
  graph_thread.start();
}

ParserView::~ParserView() {
  graph_thread.quit();
  graph_thread.wait();
  delete ui;
}

void ParserView::on_lineEdit_returnPressed() {
  fileRequester->set_parser(ui->lineEdit->text());
}

void ParserView::on_treeView_doubleClicked(const QModelIndex &index) {
  treeModel.handle_doubleclick(index);
}

void ParserView::on_undoButton_clicked() { treeModel.undo(); }

void ParserView::on_redoButton_clicked() { treeModel.redo(); }

void ParserView::keyPressEvent(QKeyEvent *event) {
  if ((event->modifiers() & Qt::ControlModifier) && event->key() == 'A' && event->type() == QEvent::KeyPress) {
      ui->treeView->expandAll();
  }
}
