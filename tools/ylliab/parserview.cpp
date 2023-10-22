#include "parserview.hpp"
#include "ui_parserview.h"

ParserView::ParserView(QWidget *parent, std::unique_ptr<FileRequester> &&req) :
    QWidget(parent),
    ui(new Ui::ParserView),
    fileRequester(std::move(req))
{
    ui->setupUi(this);
}

ParserView::~ParserView()
{
    delete ui;
}

void ParserView::setParserName(QString parserName) {
    auto model = fileRequester->create_tree_model(parserName);
    ui->treeView->setModel(model.get());
    treeModel = std::move(model);
}
