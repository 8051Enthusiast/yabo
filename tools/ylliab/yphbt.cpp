#include "color.hpp"
#include "yphbtwindow.hpp"
#include "init.hpp"

#include <QApplication>
#include <QMetaType>
#include <QtGlobal>
#include <QSettings>

int main(int argc, char **argv) {
  QApplication a(argc, argv);
  a.setOrganizationName("yabo");
  a.setOrganizationDomain("yabo-lang.org");
  a.setApplicationName("yphbt");
  init_runtime();
  QFile f(":qdarkstyle/dark/darkstyle.qss");
  if (f.exists() && f.open(QFile::ReadOnly | QFile::Text)) {
    QTextStream ts(&f);
    a.setStyleSheet(ts.readAll());
    use_dark_random_colors = true;
  }
  auto env_input = qEnvironmentVariable("YPHBT_INPUT");
  std::optional<QByteArray> input;
  if (env_input.size() > 1) {
    auto res = QByteArray::fromBase64Encoding(
        env_input.toUtf8(),
        QByteArray::Base64Encoding | QByteArray::AbortOnBase64DecodingErrors);
    if (res) {
      input = *res;
    } else {
      qWarning() << "Failed to decode YPHBT_INPUT";
    }
  }
  auto env_source = qEnvironmentVariable("YPHBT_SOURCE");
  std::optional<QString> source;
  if (env_source != "") {
    source = env_source;
  } else {
    QSettings settings;
    auto saved_source = settings.value("source").toString();
    if (!saved_source.isEmpty()) {
      source = saved_source;
    }
  }
  YphbtWindow w(nullptr, source, input);
  w.show();
  return a.exec();
}
