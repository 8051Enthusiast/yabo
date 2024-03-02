#include "compile.hpp"
#include "filecontent.hpp"

#include <QFile>
#include <QTemporaryFile>

#ifndef __EMSCRIPTEN__
#include <QProcess>

LocalCompilerRunner::LocalCompilerRunner(QString program) : program(program) {}

void LocalCompilerRunner::run() {
  auto temp_output_path =
      QString::fromStdString(tmp_file_name("yabo_", ".so").string());
  QTemporaryFile input_file;
  if (!input_file.open()) {
    emit send_error("Failed to create temporary file");
    return;
  }
  input_file.write(program.toUtf8());
  input_file.flush();
  QProcess process;
  process.start("yaboc", {input_file.fileName(), temp_output_path});
  process.waitForFinished();
  auto output = process.readAllStandardError();
  if (process.exitCode() != 0) {
    auto error_msg = QString("Failed to compile:\n") + output;
    emit send_error(error_msg);
    return;
  }
  emit compiled_file(temp_output_path);
}
#endif

RemoteCompilerRunner::RemoteCompilerRunner(QString program)
    : program(program) {}

void RemoteCompilerRunner::run() {
  auto manager = new QNetworkAccessManager(this);
  auto temp_output_path =
      QString::fromStdString(tmp_file_name("yabo_", ".so").string());
  auto receive = [this, manager, temp_output_path](QNetworkReply *reply) {
    //deleteLater();
    if (reply->attribute(QNetworkRequest::HttpStatusCodeAttribute) == 422 /* Unprocessable Entity */) {
      auto error_text = reply->readAll();
      emit send_error(QString::fromUtf8(error_text));
      return;
    }
    if (reply->error() != QNetworkReply::NoError) {
      emit send_error(reply->errorString());
      return;
    }
    auto output = reply->readAll();
    auto output_file = QFile(temp_output_path);
    if (!output_file.open(QIODevice::WriteOnly)) {
      emit send_error("Failed to create temporary file");
      return;
    }
    output_file.write(output);
    output_file.close();
    emit compiled_file(temp_output_path);
  };
  connect(manager, &QNetworkAccessManager::finished, receive);
  auto request = QNetworkRequest(QUrl("http://localhost:8000/compile"));
  request.setHeader(QNetworkRequest::ContentTypeHeader, "text/plain");
  manager->post(request, program.toUtf8());
}
