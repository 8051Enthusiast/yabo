#include "filecontent.hpp"
#include <cassert>
#include <random>
extern "C" {
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
}

#include <fstream>

FileSpan::FileSpan(const uint8_t *base,
                   std::span<const uint8_t> span) noexcept {
  if (!span.data()) {
    m_size = 0;
    m_addr = (uint64_t)-1;
    return;
  }
  auto rel = span.data() - base;
  m_addr = rel;
  m_size = span.size();
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
    content = std::span<const uint8_t>(static_cast<const uint8_t *>(ptr), size);
    return;
  }
  close(fd);

  content = std::vector<uint8_t>(size);
  auto &vec = std::get<std::vector<uint8_t>>(content);
  std::ifstream file(path, std::ios::binary);
  file.read(reinterpret_cast<char *>(vec.data()), size);
}

FileContent::FileContent(std::vector<uint8_t> vec) { content = vec; }

FileContent::~FileContent() {
  if (std::holds_alternative<std::span<const uint8_t>>(content)) {
    auto span = std::get<std::span<const uint8_t>>(content);
    munmap(const_cast<uint8_t *>(span.data()), span.size());
  }
}

std::span<const uint8_t> FileContent::span() const {
  if (std::holds_alternative<std::span<const uint8_t>>(content)) {
    return std::get<std::span<const uint8_t>>(content);
  } else {
    return std::span<const uint8_t>(std::get<std::vector<uint8_t>>(content));
  }
}

std::filesystem::path tmp_file_name(std::string prefix, std::string suffix) {
  auto tmp_root = std::filesystem::temp_directory_path();
  auto random_num = std::random_device()();
  auto tmp_file_name = prefix + std::to_string(random_num) + suffix;
  return tmp_root / tmp_file_name;
}

std::span<const uint8_t> FileContent::byte_span(FileSpan file_span) const {
  auto addr = file_span.addr();
  assert(addr <= span().size());
  if (addr) {
    return std::span(span().data() + *addr, file_span.size());
  } else {
    return std::span<const uint8_t>((const uint8_t *)nullptr, 0);
  }
}

FileSpan FileContent::file_span(std::span<const uint8_t> byte_span) const {
  if (!byte_span.data()) {
    return FileSpan();
  }
  return FileSpan(span().data(), byte_span);
}

bool FileContent::is_valid_span(FileSpan sp) const {
  return !sp.addr() || *sp.addr() + sp.size() <= span().size();
}

uint8_t FileContent::get_addr(const uint8_t *addr) const {
  auto [start, end] = slice();
  if (std::less{}(addr, start)) {
    return 0xff;
  }
  if (std::greater_equal{}(addr, end)) {
    return 0xff;
  }
  return *addr;
}

std::pair<const uint8_t *, const uint8_t *> FileContent::slice() const {
  auto start = span().begin().base();
  auto end = span().end().base();
  return {start, end};
}
