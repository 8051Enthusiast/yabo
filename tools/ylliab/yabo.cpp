#include "yabo.hpp"
#include "yabo/vtable.h"
#include <algorithm>

constexpr uint64_t DEFAULT_LEVEL = YABO_ANY;
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
  auto space2 = space;
  tmp_storage = std::make_unique<uint8_t[]>(space);
  void *ptr = tmp_storage.get();
  std::align(alignof(max_align_t), max_s, ptr, space);
  tmp_storage2 = std::make_unique<uint8_t[]>(space2);
  void *ptr2 = tmp_storage2.get();
  std::align(alignof(max_align_t), max_s, ptr2, space2);
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

SpannedVal YaboValStorage::with_span_and_return_buf(
    FileSpan span, std::function<uint64_t(void *addr, uint8_t *ret)> f) {
  auto tmp_val = tmp_storage.get();
  Slice slice{span.data(), span.data() + span.size()};
  std::memcpy(tmp_val, &slice, sizeof(Slice));
  auto val = with_return_buf([=](uint8_t *ret) { return f(tmp_val, ret); });
  std::memcpy(&slice, tmp_val, sizeof(Slice));
  auto size = slice.start - span.data();
  return SpannedVal(val, FileSpan(span.data(), size), true);
}

std::optional<YaboVal> YaboValCreator::access_field(YaboVal val,
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

std::optional<YaboVal> YaboValCreator::deref(YaboVal val) {
  if (val.is_exceptional() || val->vtable->deref_level < 256 ||
      val->vtable->head >= 0) {
    return {};
  }

  auto target_level = val->vtable->deref_level - 256;
  return storage.with_return_buf([=](uint8_t *buf) {
    return val->vtable->typecast_impl(buf, val->data,
                                      target_level | YABO_VTABLE);
  });
}

int64_t YaboValCreator::array_len(YaboVal val) {
  if (val.is_exceptional() || val->vtable->head != YABO_LOOP) {
    return 0;
  }

  auto vtable = reinterpret_cast<ArrayVTable *>(val->vtable);
  return vtable->array_len_impl(val->data);
}

std::optional<YaboVal> YaboValCreator::index(YaboVal val, size_t idx) {
  if (val.is_exceptional() || val->vtable->head != YABO_LOOP) {
    return {};
  }

  return storage.with_address_and_return_buf(
      val, [=](DynValue *val, uint8_t *buf) {
        auto vtable = reinterpret_cast<ArrayVTable *>(val->vtable);
        auto status = vtable->skip_impl(val->data, (uint64_t)idx);
        if (status)
          return status;
        return vtable->current_element_impl(buf, val->data,
                                            DEFAULT_LEVEL | YABO_VTABLE);
      });
}

static std::optional<FileSpan> primary_slice(DynValue *array, DynValue *buf) {
  auto cur = array, next = buf;
  while (true) {
    auto vtable = reinterpret_cast<const ArrayVTable *>(cur->vtable);
    auto status =
        vtable->inner_array_impl(next->data, cur->data, 0 | YABO_VTABLE);
    if (status == BACKTRACK) {
      break;
    }
    if (status != OK) {
      return {};
    }
    std::swap(cur, next);
  }
  auto slice = reinterpret_cast<Slice *>(cur->data);
  return FileSpan(reinterpret_cast<const uint8_t *>(slice->start),
                  reinterpret_cast<const uint8_t *>(slice->end));
}

std::optional<FileSpan> YaboValCreator::extent(YaboVal val) {
  if (val.kind() == YaboValKind::YABOU8) {
    auto start = val.access_u8();
    return FileSpan(start, 1);
  }
  if (val.kind() != YaboValKind::YABONOM) {
    return {};
  }
  auto vtable = reinterpret_cast<const NominalVTable *>(val->vtable);
  return storage.tmp_buf_o_plenty<std::optional<FileSpan>>(
      [=](DynValue *t1, DynValue *t2, DynValue *t3) -> std::optional<FileSpan> {
        auto start = t1;
        if (vtable->start_impl(start->data, val->data, 0 | YABO_VTABLE) != OK) {
          return {};
        }
        auto end = t2;
        if (vtable->end_impl(end->data, val->data, 0) != OK) {
          return {};
        }
        auto array_vtable =
            reinterpret_cast<const ArrayVTable *>(start->vtable);
        auto extent = t3;
        auto status = array_vtable->span_impl(extent->data, start->data,
                                              0 | YABO_VTABLE, end->data);
        if (status != OK) {
          return {};
        }
        return primary_slice(extent, t2);
      });
}

std::optional<SpannedVal> YaboValCreator::parse(ParseFun parser, FileSpan buf) {
  return storage.with_span_and_return_buf(buf, [=](void *addr, uint8_t *ret) {
    return parser(ret, nullptr, DEFAULT_LEVEL | YABO_VTABLE, addr);
  });
}
