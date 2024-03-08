#include "init.hpp"
#include "mainwindow.hpp"

#include <QApplication>
#include <QMetaType>

int main(int argc, char *argv[]) {
  QApplication a(argc, argv);
  init_meta_types();
  MainWindow w;
  w.show();
  return a.exec();
}
