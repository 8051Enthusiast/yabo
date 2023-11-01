#pragma once
#include <cstdint>
#include <filesystem>
#include <span>
#include <variant>
#include <vector>

// bytes that either come from an mmap or are read
// into a vector if mmap fails
class FileContent {
public:
  FileContent(std::filesystem::path path);
  ~FileContent();
  std::span<const uint8_t> span() const;

private:
  std::variant<std::vector<uint8_t>, std::span<const uint8_t>> content;
};

using FileRef = std::shared_ptr<FileContent>;