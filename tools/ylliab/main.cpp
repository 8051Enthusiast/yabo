#include "mainwindow.hpp"
#include "request.hpp"
#include "graph.hpp"

#include <QApplication>
#include <QMetaType>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    qRegisterMetaType<Meta>();
    qRegisterMetaType<Request>();
    qRegisterMetaType<Response>();
    qRegisterMetaType<GraphUpdate>();
    qRegisterMetaType<PositionsUpdate>();
    MainWindow w;
    w.show();
    return a.exec();
}
