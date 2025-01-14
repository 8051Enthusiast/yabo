#include "init.hpp"
#include "ylliabwindow.hpp"

#include <QApplication>
#include <QCommandLineOption>
#include <QCommandLineParser>
#include <QMetaType>
#include <QStringList>

int main(int argc, char *argv[]) {
  QApplication app(argc, argv);
  app.setOrganizationName("yabo");
  app.setOrganizationDomain("yabo-lang.org");
  app.setApplicationName("ylliab");
  QCommandLineParser options;

  options.addOption(parser_opt);
  options.addOption(input_opt);
  options.addOption(kernkeule_opt);
  options.addHelpOption();
  options.process(app);

  init_runtime();
  YlliabWindow w(options);
  w.show();
  return app.exec();
}
