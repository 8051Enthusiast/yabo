#pragma once

#include <cstdint>
#include <cstring>
#include <functional>
#include <memory>
#include <optional>
#include <span>
#include <vector>

#include <yabo/dynamic.h>
#ifdef __EMSCRIPTEN__
// https://github.com/WebAssembly/binaryen/pull/2427
#define YABO_GLOBAL_INIT "orig$yabo_global_init"
#else
#define YABO_GLOBAL_INIT "yabo_global_init"
#endif

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

static constexpr int64_t segfault_err_code = 0xff;
int init_segfault_handler();

using ByteSpan = std::span<const uint8_t>;

static bool span_contains(ByteSpan outer, ByteSpan inner) noexcept {
  return outer.data() <= inner.data() &&
         outer.data() + outer.size() >= inner.data() + inner.size();
}


// interned yabo value, can be compared by pointer comparison
struct YaboVal {
  // is not null
  const DynValue *val;
  explicit YaboVal(const DynValue *v) : val(v) {}
  bool operator==(const YaboVal &other) const noexcept {
    return std::bit_cast<uintptr_t>(val) == std::bit_cast<uintptr_t>(other.val);
  }
  const DynValue *operator->() const noexcept { return val; }
  YaboValKind kind() const noexcept;

  int64_t access_int() const noexcept { return dyn_int(val); }

  int32_t access_char() const noexcept { return dyn_char(val); }

  int8_t access_bool() const noexcept { return dyn_bit(val); }

  const uint8_t *access_u8() const noexcept { return dyn_u8(val); }

  std::optional<ptrdiff_t> field_offset(char const *name) const noexcept;

  uint64_t access_error() const noexcept { return dyn_error(val); }

  bool is_exceptional() const noexcept { return !val->vtable; }

  bool is_backtrack() const noexcept {
    return !val->vtable && access_error() == YABO_STATUS_BACKTRACK;
  }
  static constexpr char const *list_head = "this";
  static constexpr char const *list_tail = "next";
  bool is_list_block() const noexcept;
};

struct SpannedVal : public YaboVal {
  SpannedVal(YaboVal val, ByteSpan span, bool active) noexcept
      : YaboVal(val), span(span), active(active) {}
  explicit SpannedVal() noexcept
      : YaboVal(nullptr), span(ByteSpan()), active(false) {}
  ByteSpan span;
  bool active;
};

struct YaboValBytes {
  YaboValBytes(ByteSpan byte_span) noexcept : bytes(byte_span){};

  YaboValBytes(const DynValue *x) noexcept {
    bytes = std::span((uint8_t *)&x->vtable, dyn_val_size(x));
  }

  ByteSpan bytes;

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
  SpannedVal
  with_span_and_return_buf(ByteSpan span,
                           std::function<uint64_t(void *addr, uint8_t *ret)>);
  // all functions take at most 3 value arguments, so this should be enough for
  // most cases
  template <typename T>
  T tmp_buf_o_plenty(
      std::function<T(DynValue *tmp1, DynValue *tmp2, DynValue *tmp3)> f) {
    auto t1 = next_val_ptr();
    auto t2 = tmp();
    auto t3 = tmp2();
    return f(t1, t2, t3);
  }
  template <typename T>
  T tmp_buf_o_plenty(std::function<T(DynValue *tmp1, DynValue *tmp2)> f) {
    return tmp_buf_o_plenty<T>(
        [=](DynValue *t1, DynValue *t2, DynValue *) { return f(t1, t2); });
  }

private:
  size_t fresh_storage_size() const {
    auto current_len = old_storage.size();
    return std::max(2 * (max_size + alignof(max_align_t)),
                    base_alloc_size << current_len);
  }

  size_t current_free_space() const noexcept {
    return fresh_storage_size() - current_offset;
  }

  DynValue *tmp() const { return (DynValue *)tmp_storage.get(); }
  DynValue *tmp2() const { return (DynValue *)tmp_storage2.get(); }

  DynValue *next_val_ptr();
  void expand_storage();

  size_t max_size;
  size_t current_offset;
  std::unique_ptr<uint8_t[]> fresh_storage;
  // provides storage for one temporary value that gets thrown away later
  std::unique_ptr<uint8_t[]> tmp_storage;
  // tmp storage 2: electric boogaloo
  std::unique_ptr<uint8_t[]> tmp_storage2;
  std::vector<std::unique_ptr<uint8_t[]>> old_storage;

  std::unordered_map<YaboValBytes, YaboVal> interner;

  static constexpr size_t base_alloc_size = 1024;
};

template <> struct std::hash<YaboVal> {
  std::size_t operator()(const YaboVal &val) const noexcept {
    return (size_t)(val.val);
  }
};

constexpr uint64_t DEFAULT_LEVEL = YABO_ANY;
class YaboValCreator {
public:
  YaboValCreator(YaboValStorage &&store) : storage(std::move(store)) {}
  YaboValCreator() = default;
  std::optional<YaboVal> access_field(YaboVal val, const char *name,
                                      int64_t level = DEFAULT_LEVEL);
  std::optional<YaboVal> deref(YaboVal val);
  int64_t array_len(YaboVal val);
  std::optional<YaboVal> index(YaboVal val, size_t idx);
  std::optional<YaboVal> skip(YaboVal val, size_t offset);
  std::optional<SpannedVal> parse(ParseFun parser, const void *args, ByteSpan buf);
  std::optional<ByteSpan> extent(YaboVal val);

private:
  YaboValStorage storage;
};

YaboValCreator init_vals_from_lib(void *lib, std::pair<const uint8_t*, const uint8_t*> span);