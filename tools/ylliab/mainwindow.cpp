#include "mainwindow.hpp"
#include "parserview.hpp"
#include "./ui_mainwindow.h"
#include "filerequester.hpp"

#include <QFileDialog>
#include <QMessageBox>

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent)
    , ui(new Ui::MainWindow)
{
    ui->setupUi(this);
}

MainWindow::~MainWindow()
{
    delete ui;
}
void MainWindow::on_parserFileButton_clicked()
{
    auto fileName = QFileDialog::getOpenFileName(this, "Open Parser File...");
    if (fileName != "") {
        ui->parserFileEdit->setText(fileName);
    }
}


void MainWindow::on_intputFileButton_clicked()
{
    auto fileName = QFileDialog::getOpenFileName(this, "Open Input File...");
    if (fileName != "") {
        ui->inputFileEdit->setText(fileName);
    }
}


void MainWindow::on_createTabBox_accepted()
{
    auto showDialog = [](QString text) {
        QMessageBox messageBox;
        messageBox.setText(text);
        messageBox.setStandardButtons(QMessageBox::Close);
        return messageBox.exec();
    };
    if (ui->parserFileEdit->text() == "") {
        showDialog("Parser file empty!");
        return;
    }
    if (ui->inputFileEdit->text() == "") {
        showDialog("Input file empty!");
        return;
    }
    if (ui->parserNameEdit->text() == "") {
        showDialog("Parser name empty!");
        return;
    }
    auto factory = FileRequesterFactory();
    auto fileRequester = factory.create_file_requester(ui->parserFileEdit->text(), ui->inputFileEdit->text());
    auto error = fileRequester->error_message();
    if (error != "") {
        showDialog(error);
        return;
    }
    auto view = new ParserView(nullptr, std::move(fileRequester));
    view->setParserName(ui->parserNameEdit->text());
    auto name = ui->inputFileEdit->text();
    ui->tabWidget->addTab(view, name);
    auto newTabIndex = ui->tabWidget->count() - 1;
    ui->tabWidget->setCurrentIndex(newTabIndex);
}

