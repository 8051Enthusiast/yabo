#include "mainwindow.hpp"
#include "request.hpp"

#include <QApplication>
#include <QMetaType>

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    qRegisterMetaType<Meta>();
    qRegisterMetaType<Request>();
    qRegisterMetaType<Response>();
    MainWindow w;
    w.show();
    return a.exec();
}
