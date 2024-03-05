#include "graph.hpp"
#include "yphbtwindow.hpp"
#include "request.hpp"
#include "color.hpp"

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
    QFile f(":qdarkstyle/dark/darkstyle.qss");
    if (f.exists() && f.open(QFile::ReadOnly | QFile::Text)) {
        QTextStream ts(&f);
        a.setStyleSheet(ts.readAll());
        use_dark_random_colors = true;
    }
    YphbtWindow w;
    w.show();
    return a.exec();
}
