#pragma once
#include <QObject>
#include <QRunnable>
#include <QString>
#include <filesystem>
#include <memory>

#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QThreadPool>

#ifndef __EMSCRIPTEN__
class LocalCompilerRunner : public QObject, public QRunnable {
  Q_OBJECT
public:
  LocalCompilerRunner(QString program);
  ~LocalCompilerRunner() = default;
  QString program;
  void run();
signals:
  void send_error(QString error);
  void compiled_file(QString file_path);
};

template <class T>
static inline void start_local_compile(QString program, T *object,
                                       void (T::*load_compiled_file)(QString),
                                       void (T::*compile_error)(QString)) {
  auto runner = new LocalCompilerRunner(program);
  runner->setAutoDelete(true);
  object->connect(runner, &LocalCompilerRunner::compiled_file, object,
                  load_compiled_file, Qt::QueuedConnection);
  object->connect(runner, &LocalCompilerRunner::send_error, object,
                  compile_error, Qt::QueuedConnection);
  QThreadPool::globalInstance()->start(runner);
}
#endif

class RemoteCompilerRunner : public QObject {
  Q_OBJECT
public:
  RemoteCompilerRunner(QString program);
  ~RemoteCompilerRunner() = default;
  QString program;
  void run();
signals:
  void send_error(QString error);
  void compiled_file(QString file_path);
};

template <class T>
static inline void start_remote_compile(QString program, T *object,
                                        void (T::*load_compiled_file)(QString),
                                        void (T::*compile_error)(QString)) {
  auto runner = new RemoteCompilerRunner(program);
  object->connect(runner, &RemoteCompilerRunner::compiled_file, object,
                  load_compiled_file, Qt::QueuedConnection);
  object->connect(runner, &RemoteCompilerRunner::send_error, object,
                  compile_error, Qt::QueuedConnection);
  runner->run();
}