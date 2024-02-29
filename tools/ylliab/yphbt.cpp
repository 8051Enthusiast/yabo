#include "graph.hpp"
#include "yphbtwindow.hpp"
#include "request.hpp"

#include <QApplication>
#include <QMetaType>

int main(int argc, char **argv) {
    QApplication a(argc, argv);
    qRegisterMetaType<Meta>();
    qRegisterMetaType<Request>();
    qRegisterMetaType<Response>();
    qRegisterMetaType<TreeIndex>();
    qRegisterMetaType<RootIndex>();
    qRegisterMetaType<NodeRange>();
    qRegisterMetaType<GraphUpdate>();
    qRegisterMetaType<PositionsUpdate>();
    YphbtWindow w;
    w.show();
    return a.exec();
}
