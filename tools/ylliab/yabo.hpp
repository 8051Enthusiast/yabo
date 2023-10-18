#pragma once

#include <cstdint>
#include <cstring>
#include <functional>
#include <memory>
#include <optional>
#include <span>
#include <vector>

#include <yabo/dynamic.h>

enum class YaboValKind : int64_t {
  YABOINTEGER = 0x100,
  YABOBIT = 0x200,
  YABOCHAR = 0x300,
  YABOARRAY = 0x400,
  YABOPARSER = 0x500,
  YABOFUNARGS = 0x600,
  YABOBLOCK = 0x700,
  YABOUNIT = 0x800,
  YABOU8 = 0x900,
  YABONOM = -1,
  YABOERROR = -2,
};

// interned yabo value, can be compared by pointer comparison
struct YaboVal {
  // is not null
  const DynValue *val;
  explicit YaboVal(const DynValue *v) : val(v) {}
  bool operator==(const YaboVal &) const noexcept = default;
  const DynValue *operator->() const noexcept { return val; }
  YaboValKind kind();

  inline int64_t access_int() const noexcept { return *(int64_t *)val->data; }

  inline int32_t access_char() const noexcept { return *(int32_t *)val->data; }

  inline int8_t access_bool() const noexcept { return *(int8_t *)val->data; }

  inline uint64_t access_error() const noexcept { return *(uint64_t *)val->data; }

  inline bool is_exceptional() const noexcept { return !val->vtable; }

  inline bool is_backtrack() const noexcept {
    return !val->vtable && access_error() == BACKTRACK;
  }
};

struct YaboValBytes {
  YaboValBytes(std::span<uint8_t> byte_span) noexcept : bytes(byte_span){};

  YaboValBytes(const DynValue *x) noexcept {
    bytes = std::span((uint8_t *)x, dyn_val_size((DynValue *)x));
  }

  std::span<uint8_t> bytes;

  bool operator==(const YaboValBytes &rhs) const noexcept {
    if (bytes.size() != rhs.bytes.size()) {
      return false;
    }
    return std::memcmp(&bytes.front(), &rhs.bytes.front(), bytes.size()) == 0;
  }
};

template <> struct std::hash<YaboValBytes> {
  std::size_t operator()(const YaboValBytes &f) const noexcept {
    // fnv1 implementation
    uint64_t hash = 0xcbf29ce484222325;
    for (uint8_t b : f.bytes) {
      hash ^= b;
      hash *= 0x100000001b3;
    }
    return (size_t)hash;
  }
};

class YaboValStorage {
public:
  YaboValStorage(size_t max_s);
  YaboValStorage() = default;
  YaboVal with_return_buf(std::function<uint64_t(uint8_t *ret)>);
  YaboVal with_address_and_return_buf(
      YaboVal addr, std::function<uint64_t(DynValue *addr, uint8_t *ret)>);
  YaboVal
  with_span_and_return_buf(std::span<uint8_t> span,
                           std::function<uint64_t(void *addr, uint8_t *ret)>);

private:
  struct Slice {
    Slice(std::span<uint8_t> s) : start(s.data()), end(s.data() + s.size()) {}
    uint8_t *start;
    uint8_t *end;
  };

  inline size_t fresh_storage_size() const {
    auto current_len = old_storage.size();
    return std::max(2 * (max_size + alignof(max_align_t)),
                    base_alloc_size << current_len);
  }

  inline size_t current_free_space() const noexcept {
    return fresh_storage_size() - current_offset;
  }

  inline DynValue *tmp() const { return (DynValue *)tmp_storage.get(); }

  DynValue *next_val_ptr();
  void expand_storage();

  size_t max_size;
  size_t current_offset;
  std::unique_ptr<uint8_t[]> fresh_storage;
  // provides storage for one temporary value that gets thrown away later
  std::unique_ptr<uint8_t[]> tmp_storage;
  std::vector<std::unique_ptr<uint8_t[]>> old_storage;

  std::unordered_map<YaboValBytes, YaboVal> interner;

  static constexpr size_t base_alloc_size = 1024;
};

template <> struct std::hash<YaboVal> {
  std::size_t operator()(const YaboVal &val) const noexcept {
    return (size_t)(val.val);
  }
};

class YaboValCache {
public:
  YaboValCache(YaboValStorage &&store)
      : storage(std::move(store)), deref_cache({}) {}
  YaboValCache() = default;
  std::optional<YaboVal> access_field(YaboVal val, const char *name);
  std::optional<YaboVal> deref(YaboVal val);
  int64_t array_len(YaboVal val);
  std::optional<YaboVal> index(YaboVal val, size_t idx);
  std::optional<YaboVal> parse(ParseFun parser, std::span<uint8_t> buf);

private:
  YaboValStorage storage;
  std::unordered_map<YaboVal, YaboVal> deref_cache;
};
