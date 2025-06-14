#include "filecontent.hpp"
#include "yabo.hpp"
#include <cassert>
#include <map>
#include <random>
extern "C" {
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
}

#include <fstream>

struct SegmentMap : std::map<size_t, ByteSpan, std::greater<size_t>> {
  SegmentMap(ByteSpan file)
      : std::map<size_t, ByteSpan, std::greater<size_t>>({{0, file}}) {}

  SegmentMap(const std::vector<std::pair<size_t, size_t>> &map) {
    for (auto [start, len] : map) {
      auto ptr = (const uint8_t *)start;
      auto span = ByteSpan(ptr, len);
      insert({start, span});
    }
  }

  std::optional<std::pair<size_t, ByteSpan>>
  get_span(size_t addr) const noexcept {
    auto it = this->lower_bound(addr);
    if (it == end()) {
      return {};
    }
    auto [start, span] = *it;
    if (addr >= start + span.size()) {
      return {};
    }

    size_t rel_addr = addr - start;
    return {{rel_addr, span}};
  }
};

FileSpan::FileSpan(const uint8_t *base,
                   std::span<const uint8_t> span) noexcept {
  if (!span.data()) {
    m_size = 0;
    m_addr = (uint64_t)-1;
    return;
  }
  m_size = span.size();
  if (!base) {
    m_addr = (size_t)span.data();
  } else {
    auto rel = span.data() - base;
    m_addr = rel;
  }
}

FileContent::FileContent(std::filesystem::path path) {
  auto fd = open(path.c_str(), O_RDONLY);
  if (fd == -1) {
    throw std::runtime_error("could not open file");
  }
  struct stat statbuf;
  if (fstat(fd, &statbuf) == -1) {
    close(fd);
    throw std::runtime_error("could not stat file");
  }
  size_t size;
  switch (statbuf.st_mode & S_IFMT) {
  case S_IFREG:
    size = statbuf.st_size;
    break;
#ifdef linux
#include <linux/fs.h>
  case S_IFBLK: {
    int ret = ioctl(fd, BLKGETSIZE64, &size);
    if (ret == -1) {
      close(fd);
      throw std::runtime_error("could not get block device size");
    }
    break;
  }
#endif
  default:
    close(fd);
    throw std::runtime_error("unsupported file type");
  }
  auto ptr = mmap(nullptr, size, PROT_READ, MAP_SHARED, fd, 0);
  if (ptr != MAP_FAILED) {
    auto span = ByteSpan(static_cast<const uint8_t *>(ptr), size);
    storage = span;
    segment_map = std::make_unique<SegmentMap>(span);
    return;
  }
  close(fd);

  storage = std::vector<uint8_t>(size);
  auto &vec = std::get<std::vector<uint8_t>>(*storage);
  std::ifstream file(path, std::ios::binary);
  file.read(reinterpret_cast<char *>(vec.data()), size);
  auto span = ByteSpan(vec.data(), size);
  segment_map = std::make_unique<SegmentMap>(span);
}

FileContent::FileContent(std::vector<uint8_t> vec) {
  auto span = ByteSpan(vec.data(), vec.size());
  segment_map = std::make_unique<SegmentMap>(span);
  storage = std::move(vec);
}

FileContent::FileContent(const std::vector<std::pair<size_t, size_t>> &map)
    : segment_map(std::make_unique<SegmentMap>(map)), storage({}) {}

FileContent::~FileContent() {
  if (storage && std::holds_alternative<std::span<const uint8_t>>(*storage)) {
    auto span = std::get<std::span<const uint8_t>>(*storage);
    munmap(const_cast<uint8_t *>(span.data()), span.size());
  }
}

std::filesystem::path tmp_file_name(std::string prefix, std::string suffix) {
  auto tmp_root = std::filesystem::temp_directory_path();
  auto random_num = std::random_device()();
  auto tmp_file_name = prefix + std::to_string(random_num) + suffix;
  return tmp_root / tmp_file_name;
}

ByteSpan FileContent::byte_span(FileSpan file_span) const {
  auto addr = file_span.addr();
  if (addr) {
    if (!is_valid_span(file_span)) {
      return ByteSpan();
    }
    auto [rel_addr, span] = *segment_map->get_span(*addr);
    return ByteSpan(span.data() + rel_addr, file_span.size());
  } else {
    return ByteSpan();
  }
}

FileSpan FileContent::file_span(std::span<const uint8_t> byte_span) const {
  if (!byte_span.data()) {
    return FileSpan();
  }
  return FileSpan(base(), byte_span);
}

bool FileContent::is_valid_span(FileSpan sp) const {
  if (!sp) {
    return true;
  }
  auto res = segment_map->get_span(*sp.addr());
  if (!res) {
    return false;
  }
  auto [rel_addr, span] = *res;
  return rel_addr < span.size();
}

uint8_t FileContent::get_addr(const uint8_t *addr) const {
  size_t rel_addr;
  if (!base()) {
    // we hope that casting addr to (size_t) is actually
    // the same as doing addr - 0 for platforms where this is
    // viable
    rel_addr = (size_t)addr;
  } else {
    rel_addr = addr - base();
  }
  return get_addr(rel_addr);
}

std::pair<const uint8_t *, const uint8_t *> FileContent::slice() const {
  if (!base()) {
    auto start = nullptr;
    auto last_segment = segment_map->begin();
    const uint8_t *end;
    if (last_segment == segment_map->end()) {
      end = nullptr;
    } else {
      end = last_segment->second.end().base();
    }
    return {start, end};
  }
  auto start = base();
  auto end = start + end_address();
  return {start, end};
}

uint8_t FileContent::get_addr(size_t addr) const {
  auto res = segment_map->get_span(addr);
  if (!res) {
    return 0xff;
  }
  return res->second[res->first];
}

size_t FileContent::end_address() const {
  auto end = segment_map->begin();
  if (end == segment_map->end()) {
    return 0;
  }
  return end->first + end->second.size();
}

std::span<const uint8_t> FileContent::segment_from(size_t pos) const {
  auto res = segment_map->get_span(pos);
  if (!res) {
    return {};
  }
  auto [rel_addr, current_span] = *res;
  return current_span.subspan(rel_addr, current_span.size() - rel_addr);
}

const uint8_t *FileContent::base() const {
  if (!storage) {
    return nullptr;
  }
  if (std::holds_alternative<std::span<const uint8_t>>(*storage)) {
    return std::get<std::span<const uint8_t>>(*storage).data();
  } else {
    return std::span<const uint8_t>(std::get<std::vector<uint8_t>>(*storage))
        .data();
  }
}

size_t FileContent::total_size() const {
  auto [start, end] = slice();
  return end - start;
}
