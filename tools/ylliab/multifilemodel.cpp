#include "multifilemodel.hpp"
#include "yabo.hpp"
#include <QFileDialog>
#include <QMessageBox>
#include <algorithm>
#include <memory>
#include <ranges>
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
    auto parsed = vals.parse(parse_fun, nullptr, span);
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

std::strong_ordering compare(const FileOffsets &offsets,
                             const std::vector<FileInfo> &files, size_t lhs,
                             size_t rhs) {
  if (lhs == rhs) {
    return std::strong_ordering::equal;
  }
  auto lhs_valid = offsets.is_valid[lhs];
  auto rhs_valid = offsets.is_valid[rhs];
  if (lhs_valid != rhs_valid) {
    return lhs_valid > rhs_valid ? std::strong_ordering::less
                                 : std::strong_ordering::greater;
  }

  auto subspan = [&](size_t idx) {
    auto [start, end] = files[idx].file_ref->slice();
    return ByteSpan(start + offsets.offsets[idx], end);
  };

  auto lhs_span = subspan(lhs);
  auto rhs_span = subspan(rhs);
  return std::lexicographical_compare_three_way(
      lhs_span.begin(), lhs_span.end(), rhs_span.begin(), rhs_span.end());
}

FileOrder find_order(const std::vector<FileInfo> &files,
                     const FileOffsets &offsets) {
  FileOrder order;
  order.ordered_to_index.reserve(files.size());
  order.index_to_ordered.resize(files.size());
  for (size_t i = 0; i < files.size(); ++i) {
    order.ordered_to_index.push_back(i);
  }

  std::stable_sort(order.ordered_to_index.begin(), order.ordered_to_index.end(),
                   [&](size_t lhs, size_t rhs) {
                     return compare(offsets, files, lhs, rhs) ==
                            std::strong_ordering::less;
                   });

  for (size_t i = 0; i < order.ordered_to_index.size(); ++i) {
    order.index_to_ordered[order.ordered_to_index[i]] = i;
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

  auto parse_fun = *(ParseFun *)dlsym(handle, parser_name.toStdString().c_str());
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

std::unique_ptr<MultiFileParser>
ask_files_for_multi_file_parser(QWidget *parent) {
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

ByteSpan FileUpdate::span_at_index(size_t idx) const {
  auto [start, end] = files->at(idx).file_ref->slice();
  return ByteSpan(start + offsets.offsets.at(idx), end);
}

size_t FileUpdate::valid_end() const {
  auto invalid_indices =
      order.ordered_to_index | std::views::transform([&](size_t idx) {
        return std::pair{!offsets.is_valid[idx], order.index_to_ordered[idx]};
      });

  auto bound = std::ranges::lower_bound(invalid_indices, true, {},
                                        [](const auto &t) { return t.first; });
  if (bound == invalid_indices.end()) {
    return order.ordered_to_index.size();
  }
  return (*bound).second;
}

size_t FileUpdate::lower_bound(ByteSpan bytes) const {
  auto valid_count = valid_end();
  auto spans =
      order.ordered_to_index | std::views::take(valid_count) |
      std::views::transform([&](size_t idx) {
        return std::pair{span_at_index(idx), order.index_to_ordered[idx]};
      });

  auto cmp = [](const auto &element, const auto &value) {
    return std::lexicographical_compare(element.begin(), element.end(),
                                        value.begin(), value.end());
  };

  auto bound = std::ranges::lower_bound(spans, bytes, cmp,
                                        [](const auto &t) { return t.first; });

  if (bound == spans.end()) {
    return valid_end();
  }

  return (*bound).second;
}

size_t FileUpdate::upper_bound(ByteSpan bytes) const {
  auto valid_count = valid_end();
  auto spans =
      order.ordered_to_index | std::views::take(valid_count) |
      std::views::transform([&](size_t idx) {
        return std::pair{span_at_index(idx), order.index_to_ordered[idx]};
      });

  auto cmp = [](const auto &value, const auto &element) {
    // make sure that if the value is a prefix of element, they're treated
    // as if equal
    auto common_prefix_size = std::min(value.size(), element.size());
    auto shortened_element = element.subspan(0, common_prefix_size);
    auto res = std::lexicographical_compare(value.begin(), value.end(),
                                            shortened_element.begin(),
                                            shortened_element.end());

    return res;
  };

  auto bound = std::ranges::upper_bound(spans, bytes, cmp,
                                        [](const auto &t) { return t.first; });

  if (bound == spans.end()) {
    return valid_end();
  }

  return (*bound).second;
}
