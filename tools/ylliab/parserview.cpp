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
