#include "init.hpp"
#include "ylliabwindow.hpp"

#include <QApplication>
#include <QMetaType>

int main(int argc, char *argv[]) {
  QApplication a(argc, argv);
  init_runtime();
  YlliabWindow w;
  w.show();
  return a.exec();
}
