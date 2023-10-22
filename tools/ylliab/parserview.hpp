#ifndef PARSERVIEW_H
#define PARSERVIEW_H

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

private:
    Ui::ParserView *ui;
    std::unique_ptr<FileRequester> fileRequester;
    std::unique_ptr<YaboTreeModel> treeModel;
};

#endif // PARSERVIEW_H
