#include "parserview.hpp"
#include "ui_parserview.h"

ParserView::ParserView(QWidget *parent, std::unique_ptr<FileRequester> &&req) :
    QWidget(parent),
    ui(new Ui::ParserView),
    fileRequester(std::move(req)),
    treeModel(fileRequester->get_tree_model())
{
    ui->setupUi(this);
    ui->treeView->setModel(&treeModel);
}

ParserView::~ParserView()
{
    delete ui;
}

void ParserView::on_lineEdit_returnPressed()
{
    fileRequester->set_parser(ui->lineEdit->text());
}


void ParserView::on_treeView_doubleClicked(const QModelIndex &index)
{
    treeModel.handle_doubleclick(index);
}


void ParserView::on_undoButton_clicked()
{
    treeModel.undo();
}


void ParserView::on_redoButton_clicked()
{
    treeModel.redo();
}

