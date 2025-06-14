#pragma once
#include <cstdint>
#include <filesystem>
#include <optional>
#include <span>
#include <variant>
#include <vector>

struct SegmentMap;

class FileSpan {
public:
  FileSpan(const uint8_t *base, std::span<const uint8_t> span) noexcept;
  FileSpan(uint64_t addr, uint64_t size) noexcept : m_size(size), m_addr(addr) {}
  FileSpan() noexcept : m_size(0), m_addr(-1) {}
  std::optional<uint64_t> addr() const noexcept {
    if (m_addr == (uint64_t)-1) {
      return {};
    }
    return m_addr;
  }
  uint64_t size() const noexcept { return m_size; }
  operator bool() const noexcept { return m_addr != (uint64_t)-1; }

private:
  uint64_t m_size;
  uint64_t m_addr;
};

// bytes that either come from an mmap or are read
// into a vector if mmap fails
class FileContent {
public:
  FileContent(std::filesystem::path path);
  FileContent(std::vector<uint8_t> vec);
  FileContent(const std::vector<std::pair<size_t, size_t>> &map);
  ~FileContent();
  std::span<const uint8_t> byte_span(FileSpan file_span) const;
  FileSpan file_span(std::span<const uint8_t> byte_span) const;
  std::span<const uint8_t> segment_from(size_t pos) const;
  size_t end_address() const;
  uint8_t get_addr(size_t addr) const;
  uint8_t get_addr(const uint8_t *addr) const;
  std::pair<const uint8_t *, const uint8_t *> slice() const;
  size_t total_size() const;
  bool is_valid_span(FileSpan sp) const;

private:
  const uint8_t *base() const;
  std::unique_ptr<SegmentMap> segment_map;
  std::optional<std::variant<std::vector<uint8_t>, std::span<const uint8_t>>>
      storage;
};

using FileRef = std::shared_ptr<FileContent>;

std::filesystem::path tmp_file_name(std::string prefix, std::string suffix);