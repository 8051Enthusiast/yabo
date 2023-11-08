#include "parserview.hpp"
#include "hex.hpp"
#include "ui_parserview.h"
#include "yabotreemodel.hpp"

#include <QKeyEvent>
#include <QObject>
#include <QOpenGLWidget>

#include <QHeaderView>
#include <iostream>

ParserView::ParserView(QWidget *parent, std::unique_ptr<FileRequester> &&req)
    : QWidget(parent), ui(new Ui::ParserView), fileRequester(std::move(req)),
      treeModel(fileRequester->get_tree_model()) {
  ui->setupUi(this);
  ui->treeView->setModel(&treeModel);
  auto file = fileRequester->file_ref();
  auto hex_model = new HexTableModel(file, fileRequester.get());
  ui->tableView->setModel(hex_model);
  QFont hexfont("Monospace");
  hexfont.setStyleHint(QFont::TypeWriter);
  hexfont.setPointSize(12);
  ui->tableView->setFont(hexfont);
  auto delegate = new HexCell(hexfont);
  auto size = delegate->get_cell_size();
  auto vert_header = ui->tableView->verticalHeader();
  vert_header->setMinimumSectionSize(size.height());
  vert_header->setSectionResizeMode(QHeaderView::Fixed);
  vert_header->setDefaultSectionSize(size.height());
  auto horiz_header = ui->tableView->horizontalHeader();
  horiz_header->setMinimumSectionSize(size.width());
  horiz_header->setSectionResizeMode(QHeaderView::Fixed);
  horiz_header->setDefaultSectionSize(size.width());
  ui->tableView->setItemDelegate(delegate);
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
  connect(fileRequester.get(), &FileRequester::new_node, hex_model,
          &HexTableModel::add_range);
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

void ParserView::on_lineEdit_returnPressed() {
  fileRequester->set_parser(ui->lineEdit->text());
}

void ParserView::on_treeView_doubleClicked(const QModelIndex &index) {
  treeModel.handle_doubleclick(index);
}

void ParserView::on_undoButton_clicked() { treeModel.undo(); }

void ParserView::on_redoButton_clicked() { treeModel.redo(); }

void ParserView::keyPressEvent(QKeyEvent *event) {
  if ((event->modifiers() & Qt::ControlModifier) && event->key() == 'A' &&
      event->type() == QEvent::KeyPress) {
    ui->treeView->expandAll();
  }
}
