#include "yabo.hpp"
constexpr uint64_t DEFAULT_LEVEL = 0;
YaboValKind YaboVal::kind() {
  if (!val->vtable) {
    return YaboValKind::YABOERROR;
  }

  if (val->vtable->head < 0) {
    return YaboValKind::YABONOM;
  }

  return static_cast<YaboValKind>(val->vtable->head);
}

YaboValStorage::YaboValStorage(size_t max_s) {
  max_size = max_s;
  current_offset = 0;
  old_storage = std::vector<std::unique_ptr<uint8_t[]>>{};
  fresh_storage = std::make_unique<uint8_t[]>(fresh_storage_size());
  auto space = max_s + alignof(max_align_t);
  tmp_storage = std::make_unique<uint8_t[]>(space);
  void *ptr = tmp_storage.get();
  std::align(alignof(max_align_t), max_s, ptr, space);
};

DynValue *YaboValStorage::next_val_ptr() {
  auto next_ptr = static_cast<void *>(fresh_storage.get() + current_offset);
  auto old_space_left = current_free_space();
  auto new_space_left = old_space_left;
  auto res =
      std::align(alignof(max_align_t), max_size, next_ptr, new_space_left);
  if (!res) {
    expand_storage();
    // this should only recurse once since we have enough space for at least one
    // value plus padding
    return next_val_ptr();
  }
  auto diff = old_space_left - new_space_left;
  current_offset += diff;
  return reinterpret_cast<DynValue *>(res);
}

void YaboValStorage::expand_storage() {
  old_storage.push_back(std::move(fresh_storage));
  fresh_storage = std::make_unique<uint8_t[]>(fresh_storage_size());
  current_offset = 0;
}

YaboVal
YaboValStorage::with_return_buf(std::function<uint64_t(uint8_t *ret)> f) {
  auto ptr = next_val_ptr();
  auto status = f(reinterpret_cast<uint8_t *>(ptr->data));
  if (status) {
    dyn_invalidate(ptr, status);
  } else {
    // mask the value to make potential undefined padding bits disappear
    // this makes it possible to just compare and hash the underlying bytes
    // without caring about the concrete fields
    ptr->vtable->mask_impl(ptr->data);
  }
  auto bytes = YaboValBytes(ptr);
  if (interner.contains(bytes)) {
    return interner.at(bytes);
  } else {
    // this is already interned now since there is no other matching value
    // hence it is allowed to be put inside an YaboVal
    auto interned = YaboVal(ptr);
    interner.insert({bytes, interned});
    // we commit the memory as we now know it is necessary and not a duplicate
    // we don't need a bounds check as we keep a max_size area free at the end
    current_offset += dyn_val_size(ptr);
    return interned;
  }
}

YaboVal YaboValStorage::with_address_and_return_buf(
    YaboVal addr, std::function<uint64_t(DynValue *addr, uint8_t *ret)> f) {
  auto tmp_val = tmp();
  dyn_copy(tmp_val, addr.val);
  return with_return_buf([=](uint8_t *ret) { return f(tmp_val, ret); });
}

YaboVal YaboValStorage::with_span_and_return_buf(
    std::span<uint8_t> span,
    std::function<uint64_t(void *addr, uint8_t *ret)> f) {
  auto tmp_val = tmp_storage.get();
  Slice slice(span);
  std::memcpy(tmp_val, &slice, sizeof(Slice));
  return with_return_buf([=](uint8_t *ret) { return f(tmp_val, ret); });
}

std::optional<YaboVal> YaboValCache::access_field(YaboVal val,
                                                  const char *name) {
  if (val.is_exceptional() || val->vtable->head != YABO_BLOCK) {
    return {};
  }

  auto vtable = reinterpret_cast<BlockVTable *>(val->vtable);
  auto start = vtable->fields->fields;
  auto end = start + vtable->fields->number_fields;
  auto cmp = [](const char *lhs, const char *rhs) {
    return std::strcmp(lhs, rhs) < 0;
  };
  auto res = std::lower_bound(start, end, name, cmp);
  if (res == end || std::strcmp(name, *res)) {
    return {};
  }
  ptrdiff_t offset = res - start;
  auto impl = vtable->access_impl[offset];
  auto ret = storage.with_return_buf([=](uint8_t *buf) {
    return impl(buf, val->data, DEFAULT_LEVEL | YABO_VTABLE);
  });
  if (ret.is_backtrack()) {
    return {};
  }
  return ret;
}

std::optional<YaboVal> YaboValCache::deref(YaboVal val) {
  if (val.is_exceptional() || val->vtable->deref_level < 256) {
    return {};
  }

  if (deref_cache.contains(val)) {
    return deref_cache.at(val);
  }

  auto target_level = val->vtable->deref_level - 256;
  return storage.with_return_buf([=](uint8_t *buf) {
    return val->vtable->typecast_impl(buf, val->data,
                                      target_level | YABO_VTABLE);
  });
}

int64_t YaboValCache::array_len(YaboVal val) {
  if (val.is_exceptional() || val->vtable->head != YABO_LOOP) {
    return 0;
  }

  auto vtable = reinterpret_cast<ArrayVTable *>(val->vtable);
  return vtable->array_len_impl(val->data);
}

std::optional<YaboVal> YaboValCache::index(YaboVal val, size_t idx) {
  if (val.is_exceptional() || val->vtable->head != YABO_LOOP) {
    return {};
  }

  return storage.with_address_and_return_buf(val, [=](DynValue *val,
                                                      uint8_t *buf) {
    auto vtable = reinterpret_cast<ArrayVTable *>(val->vtable);
    auto status = vtable->skip_impl(val->data, (uint64_t)idx);
    if (status)
      return status;
    return vtable->current_element_impl(buf, val->data, DEFAULT_LEVEL | YABO_VTABLE);
  });
}

std::optional<YaboVal> YaboValCache::parse(ParseFun parser,
                                           std::span<uint8_t> buf) {
  return storage.with_span_and_return_buf(buf, [=](void *addr, uint8_t *ret) {
    return parser(ret, nullptr, DEFAULT_LEVEL | YABO_VTABLE, addr);
  });
}