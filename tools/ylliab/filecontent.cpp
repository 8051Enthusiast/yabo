#include "filecontent.hpp"
#include "filerequester.hpp"
extern "C" {
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
}

#include <fstream>

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