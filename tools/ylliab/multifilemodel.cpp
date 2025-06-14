#include "multifilemodel.hpp"
#include "yabo.hpp"
#include <QFileDialog>
#include <QMessageBox>
#include <memory>
extern "C" {
#include <dlfcn.h>
}

namespace {
FileOffsets find_offsets(YaboValCreator &vals, ParseFun parse_fun,
                         const std::vector<FileInfo> &files) {
  FileOffsets offsets;
  offsets.offsets.reserve(files.size());
  offsets.is_valid.resize(files.size(), true);
  for (const auto &file : files) {
    auto [start, end] = file.file_ref->slice();
    auto span = ByteSpan(start, end);
    auto parsed = vals.parse(parse_fun, span);
    if (parsed->is_exceptional()) {
      offsets.is_valid[&file - &files[0]] = false;
      offsets.offsets.push_back(0);
    } else {
      auto end_offset = parsed->span.size();
      offsets.offsets.push_back(end_offset);
    }
  }

  return offsets;
}

FileOrder find_order(const std::vector<FileInfo> &files,
                     const FileOffsets &offsets) {
  FileOrder order;
  order.ordered_to_index.reserve(files.size());
  order.index_to_ordered.reserve(files.size());
  for (size_t i = 0; i < files.size(); ++i) {
    order.ordered_to_index.push_back(i);
  }

  auto subspan = [&files, &offsets](size_t idx) {
    auto [start, end] = files[idx].file_ref->slice();
    return ByteSpan(start + offsets.offsets[idx], end);
  };

  auto cmp = [&offsets, subspan](size_t a, size_t b) {
    if (a == b) {
      return false;
    }
    auto a_valid = offsets.is_valid[a];
    auto b_valid = offsets.is_valid[b];
    if (!b_valid || !a_valid) {
      return a_valid > b_valid;
    }
    auto span_a = subspan(a);
    auto span_b = subspan(b);
    return std::lexicographical_compare(span_a.begin(), span_a.end(),
                                        span_b.begin(), span_b.end());
  };

  std::stable_sort(order.ordered_to_index.begin(), order.ordered_to_index.end(),
                   cmp);

  for (size_t i = 0; i < order.ordered_to_index.size(); ++i) {
    order.index_to_ordered.push_back(order.ordered_to_index[i]);
  }
  return order;
}
} // namespace

void MultiFileParser::new_library(QString lib_path) {
  void *new_handle = dlopen(lib_path.toStdString().c_str(), RTLD_LAZY);
  if (!new_handle) {
    emit error(QString("Failed to open library '%1': %2")
                   .arg(lib_path)
                   .arg(dlerror()));
    return;
  } else {
    if (handle) {
      dlclose(handle);
    }
    handle = new_handle;
  }

  parse_fun = *(ParseFun *)dlsym(handle, parser_name.toStdString().c_str());
  if (!parse_fun) {
    emit error(QString("Failed to find parse function in library '%1': %2")
                   .arg(lib_path)
                   .arg(dlerror()));
    return;
  }
  vals = init_vals_from_lib(handle, {nullptr, nullptr});

  auto offsets = find_offsets(vals, parse_fun, *files);
  auto order = find_order(*files, offsets);
  FileUpdate update{.files = files, .offsets = offsets, .order = order};
  emit files_updated(std::make_shared<FileUpdate>(std::move(update)));
}

MultiFileParser::MultiFileParser(std::vector<FileInfo> files,
                                 QString parser_name, QObject *parent)
    : QObject(parent),
      files(std::make_shared<std::vector<FileInfo>>(std::move(files))),
      handle(nullptr), parser_name(std::move(parser_name)) {}

std::unique_ptr<MultiFileParser> ask_files_for_multi_file_parser(QWidget *parent) {
  auto fileNames = QFileDialog::getOpenFileNames(
      parent, "Select Input Files...", QString(), "All Files (*)");
  if (fileNames.isEmpty()) {
    return nullptr;
  }
  std::vector<FileInfo> selected_files;

  for (const auto &fileName : fileNames) {
    if (!fileName.isEmpty()) {
      try {
        auto file_ref = std::make_shared<FileContent>(fileName.toStdString());
        FileInfo info;
        info.file_ref = file_ref;
        info.file_name = QFileInfo(fileName).fileName();
        selected_files.push_back(info);
      } catch (const std::exception &e) {
        QMessageBox::warning(
            parent, "File Error",
            QString("Failed to load file %1: %2").arg(fileName).arg(e.what()));
        return nullptr;
      }
    }
  }

  return std::make_unique<MultiFileParser>(std::move(selected_files), "main");
}