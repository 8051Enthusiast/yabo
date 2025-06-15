#pragma once

#include "filecontent.hpp"
#include "yabo.hpp"
#include <QObject>
#include <yabo/vtable.h>

struct FileInfo {
  FileRef file_ref;
  QString file_name;
};

struct FileOrder {
  std::vector<size_t> ordered_to_index;
  std::vector<size_t> index_to_ordered;
};

struct FileOffsets {
  std::vector<size_t> offsets;
  std::vector<bool> is_valid;
};

struct FileUpdate {
  std::shared_ptr<std::vector<FileInfo>> files;
  FileOffsets offsets;
  FileOrder order;
  ByteSpan span_at_index(size_t index) const;
  size_t valid_end() const;
  size_t lower_bound(ByteSpan bytes) const;
  size_t upper_bound(ByteSpan bytes) const;
};

class MultiFileParser : public QObject {
  Q_OBJECT
public:
  explicit MultiFileParser(std::vector<FileInfo> files,
                           QString parser_name = "main",
                           QObject *parent = nullptr);

public slots:
  void new_library(QString lib_path);

signals:
  void files_updated(std::shared_ptr<FileUpdate> update);
  void error(QString error_message);

private:
  std::shared_ptr<std::vector<FileInfo>> files;
  YaboValCreator vals{};
  QString parser_name;
  ParseFun parse_fun{};
  void *handle{};
};

std::unique_ptr<MultiFileParser>
ask_files_for_multi_file_parser(QWidget *parent);