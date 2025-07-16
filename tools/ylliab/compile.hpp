#pragma once
#include <QObject>
#include <QRunnable>
#include <QString>

#include <QThreadPool>
#include <QUrl>

enum class SourceKind {
  Content,
  Path,
};

class LocalCompilerRunner : public QObject, public QRunnable {
  Q_OBJECT
public:
  LocalCompilerRunner(QString source, SourceKind kind);
  ~LocalCompilerRunner() = default;
  QString source;
  SourceKind kind;
  void run();
signals:
  void send_error(QString error);
  void compiled_file(QString file_path);
};

template <class T>
static inline void start_local_compile(QString source, SourceKind kind,
                                       T *object,
                                       void (T::*load_compiled_file)(QString),
                                       void (T::*compile_error)(QString)) {
  auto runner = new LocalCompilerRunner(source, kind);
  runner->setAutoDelete(true);
  object->connect(runner, &LocalCompilerRunner::compiled_file, object,
                  load_compiled_file, Qt::QueuedConnection);
  object->connect(runner, &LocalCompilerRunner::send_error, object,
                  compile_error, Qt::QueuedConnection);
  QThreadPool::globalInstance()->start(runner);
}

class RemoteCompilerRunner : public QObject {
  Q_OBJECT
public:
  RemoteCompilerRunner(QString program, QUrl url);
  ~RemoteCompilerRunner() = default;
  QString program;
  QUrl url;
  void run();
signals:
  void send_error(QString error);
  void compiled_file(QString file_path);
};

template <class T>
static inline void start_remote_compile(QUrl url, QString program, T *object,
                                        void (T::*load_compiled_file)(QString),
                                        void (T::*compile_error)(QString)) {
  auto runner = new RemoteCompilerRunner(program, url);
  object->connect(runner, &RemoteCompilerRunner::compiled_file, object,
                  load_compiled_file, Qt::QueuedConnection);
  object->connect(runner, &RemoteCompilerRunner::send_error, object,
                  compile_error, Qt::QueuedConnection);
  runner->run();
}