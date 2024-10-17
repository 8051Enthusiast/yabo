#include "color.hpp"
#include "init.hpp"
#include "yphbtwindow.hpp"

#include <QApplication>
#include <QMetaType>
#include <QSettings>
#include <QStringView>
#include <QtGlobal>

static std::optional<QByteArray> decode(const QString &input) {
  if (input.isEmpty()) {
    return {};
  }
  const static QString RAW = QString("r");
  const static QString ZLIB = QString("z");
  auto excl = input.indexOf('!');
  if (excl == -1) {
    return {};
  }

  auto prefix = QStringView(input).first(excl);
  auto data = QStringView(input).sliced(excl + 1);
  if (prefix != RAW && prefix != ZLIB) {
    return {};
  }
  auto base64_data = data.toUtf8();
  auto res = QByteArray::fromBase64Encoding(
      base64_data,
      QByteArray::Base64UrlEncoding | QByteArray::AbortOnBase64DecodingErrors);
  if (!res) {
    return {};
  }

  if (prefix == RAW) {
    return res.decoded;
  } else {
    return qUncompress(res.decoded);
  }
}

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
    use_dark_colors = true;
  }
  auto env_input = qEnvironmentVariable("YPHBT_INPUT");
  std::optional<QByteArray> input = decode(env_input);
  auto env_source = qEnvironmentVariable("YPHBT_SOURCE");
  std::optional<QString> source = decode(env_source);
  YphbtWindow w(nullptr, source, input);
  w.show();
  return a.exec();
}
