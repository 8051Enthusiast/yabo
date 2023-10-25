#pragma once
#include <QWidget>
#include "filerequester.hpp"

namespace Ui {
class ParserView;
}

class ParserView : public QWidget
{
    Q_OBJECT

public:
    explicit ParserView(QWidget *parent, std::unique_ptr<FileRequester> &&req);
    ~ParserView();
    void setParserName(QString fileRequester);

private slots:
    void on_lineEdit_returnPressed();

    void on_treeView_doubleClicked(const QModelIndex &index);

    void on_undoButton_clicked();

    void on_redoButton_clicked();

private:
    Ui::ParserView *ui;
    std::unique_ptr<FileRequester> fileRequester;
    YaboTreeModel &treeModel;
};
