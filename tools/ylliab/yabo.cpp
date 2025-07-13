#include "yabo.hpp"
#include "yabo/vtable.h"
#include <algorithm>
#include <dlfcn.h>

// -- signal handling --
#if defined(__linux__)
#include <atomic>
#include <csetjmp>
#include <csignal>
// note that we rely on the local-exec linker optimization so that no
// signal-unsafe functions are called during access to segfault_resume
thread_local static std::atomic<sigjmp_buf *> segfault_resume;

// in case of stack overflows, we don't want to crash the whole application,
// but instead return an error value. this function wraps a function call so
// that, when it segfaults, it instead returns an error value. note that the
// function `f` should be a pointer to a function from a yabo library because
// these invariants need to be upheld:
// * no calls (direct or indirect) to any signal-unsafe functions, as the
//   segfault handler will siglongjmp out of the handler
// * on error, any memory that the function would have written to must be
//   assumed invalid as the segfault might have occured during an incomplete
//   write
// * as it uses longjmp, there must only be trivially destructible objects
//   anywhere in the function
// * catch_segfault should probably not be inlined as i don't know what the
//   implications of that are
template <typename T, typename... Args>
__attribute__((noinline)) static T catch_segfault(T (*f)(Args...), T err_return,
                                                  Args... args) {
  sigjmp_buf env;
  if (!sigsetjmp(env, 1)) {
    // i don't think this is an issue in practice since compilers are
    // very reluctant to optimize function calls that return multiple times,
    // but just in case this is here to make sure everything is written into
    // the sigjmp_buf before we store the pointer in segfault_resume
    std::atomic_signal_fence(std::memory_order_release);
    // even though segfault_resume is a thread-local, we still do atomic
    // writes as we could get an external SIGSEGV signal during the write
    // which could tear if it wasn't atomic and cause the handler to jump
    // to an invalid location
    segfault_resume.store(&env, std::memory_order_relaxed);
    T err = f(args...);
    segfault_resume.store(nullptr, std::memory_order_relaxed);
    return err;
  } else {
    segfault_resume.store(nullptr, std::memory_order_relaxed);
    return err_return;
  }
}

static void handler(int sig) {
  sigjmp_buf *resume = segfault_resume.load(std::memory_order_relaxed);
  if (resume) {
    siglongjmp(*resume, 1);
  } else {
    // in this case we can only call signal-safe functions
    static constinit char msg[] = "Segmentation fault\n";
    write(STDERR_FILENO, msg, sizeof(msg) - 1);
    _exit(128 + sig);
  }
}

int init_segfault_handler() {
  struct sigaction action;
  action.sa_handler = handler;
  sigfillset(&action.sa_mask);
  action.sa_flags = SA_ONSTACK;
  int err = sigaction(SIGSEGV, &action, nullptr);
  if (err) {
    return err;
  }
  return sigaction(SIGBUS, &action, nullptr);
}

#else

template <typename T, typename... Args>
static T catch_segfault(T (*f)(Args...), T err_return, Args... args) {
  return f(args...);
}

int init_segfault_handler() { return 0; }
#endif

YaboValKind YaboVal::kind() const noexcept {
  if (!val->vtable) {
    return YaboValKind::YABOERROR;
  }

  if (val->vtable->head < 0) {
    return YaboValKind::YABONOM;
  }

  return static_cast<YaboValKind>(val->vtable->head & YABO_DISC_MASK);
}

std::optional<ptrdiff_t>
YaboVal::field_offset(char const *name) const noexcept {
  if (kind() != YaboValKind::YABOBLOCK) {
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
  return res - start;
}

bool YaboVal::is_list_block() const noexcept {
  if (kind() != YaboValKind::YABOBLOCK)
    return false;
  auto block_vtable = reinterpret_cast<BlockVTable *>(val->vtable);
  if (block_vtable->fields->number_fields != 2)
    return false;
  return std::strcmp(block_vtable->fields->fields[0], list_tail) == 0 &&
         std::strcmp(block_vtable->fields->fields[1], list_head) == 0;
}

YaboValStorage::YaboValStorage(size_t max_s) {
  max_size = max_s;
  current_offset = 0;
  old_storage = std::vector<std::unique_ptr<uint8_t[]>>{};
  fresh_storage = std::make_unique<uint8_t[]>(fresh_storage_size());
  auto space = max_s + YABO_MAX_ALIGN;
  auto space2 = space;
  tmp_storage = std::make_unique<uint8_t[]>(space);
  void *ptr = tmp_storage.get();
  std::align(YABO_MAX_ALIGN, max_s, ptr, space);
  tmp_storage2 = std::make_unique<uint8_t[]>(space2);
  void *ptr2 = tmp_storage2.get();
  std::align(YABO_MAX_ALIGN, max_s, ptr2, space2);
}

DynValue *YaboValStorage::next_val_ptr() {
  auto next_ptr = static_cast<void *>(fresh_storage.get() + current_offset);
  auto old_space_left = current_free_space();
  auto new_space_left = old_space_left;
  auto res = std::align(YABO_MAX_ALIGN, max_size, next_ptr, new_space_left);
  if (!res) {
    expand_storage();
    // this should only recurse once since we have enough space for at least
    // one value plus padding
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
    ByteSpan span, std::function<uint64_t(void *addr, uint8_t *ret)> f) {
  auto tmp_val = tmp_storage.get();
  Slice slice{span.data(), span.data() + span.size()};
  std::memcpy(tmp_val, &slice, sizeof(Slice));
  auto val = with_return_buf([=](uint8_t *ret) { return f(tmp_val, ret); });
  std::memcpy(&slice, tmp_val, sizeof(Slice));
  auto size = slice.start - span.data();
  return SpannedVal(val, ByteSpan(span.data(), size), true);
}

std::optional<YaboVal>
YaboValCreator::access_field(YaboVal val, const char *name, int64_t level) {
  if (val.kind() != YaboValKind::YABOBLOCK) {
    return {};
  }

  auto maybe_offset = val.field_offset(name);
  if (!maybe_offset) {
    return {};
  }
  auto offset = *maybe_offset;
  auto vtable = reinterpret_cast<BlockVTable *>(val->vtable);
  auto impl = vtable->access_impl[offset];
  auto ret = storage.with_return_buf([=](uint8_t *buf) {
    return catch_segfault(impl, segfault_err_code, (void *)buf,
                          (const void *)val->data, level | YABO_VTABLE);
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
    return catch_segfault(val->vtable->typecast_impl, segfault_err_code,
                          (void *)buf, (const void *)val->data,
                          target_level | YABO_VTABLE);
  });
}

int64_t YaboValCreator::array_len(YaboVal val) {
  if (val.kind() != YaboValKind::YABOARRAY) {
    return 0;
  }

  auto vtable = reinterpret_cast<ArrayVTable *>(val->vtable);
  return vtable->array_len_impl(val->data);
}

std::optional<YaboVal> YaboValCreator::index(YaboVal val, size_t idx) {
  if (val.kind() != YaboValKind::YABOARRAY) {
    return {};
  }

  return storage.with_address_and_return_buf(
      val, [=](DynValue *val, uint8_t *buf) {
        auto vtable = reinterpret_cast<ArrayVTable *>(val->vtable);
        auto status = catch_segfault(vtable->skip_impl, segfault_err_code,
                                     (void *)val->data, (uint64_t)idx);
        if (status)
          return status;
        return catch_segfault(vtable->current_element_impl, segfault_err_code,
                              (void *)buf, (const void *)val->data,
                              DEFAULT_LEVEL | YABO_VTABLE);
      });
}

std::optional<YaboVal> YaboValCreator::skip(YaboVal val, size_t offset) {
  if (val.kind() != YaboValKind::YABOARRAY) {
    return {};
  }

  return storage.with_address_and_return_buf(
      val, [=](DynValue *val, uint8_t *buf) {
        auto vtable = reinterpret_cast<ArrayVTable *>(val->vtable);
        auto status = catch_segfault(vtable->skip_impl, segfault_err_code,
                                     (void *)val->data, (uint64_t)offset);
        if (status)
          return status;
        return catch_segfault(vtable->head.typecast_impl, segfault_err_code,
                              (void *)buf, (const void *)val->data,
                              0 | YABO_VTABLE);
      });
}

static std::optional<ByteSpan> primary_slice(DynValue *array, DynValue *buf) {
  auto cur = array, next = buf;
  while (cur->vtable->head != YABO_SLICEPTR) {
    auto vtable = reinterpret_cast<const ArrayVTable *>(cur->vtable);
    auto status = catch_segfault(vtable->inner_array_impl, segfault_err_code,
                                 (void *)next->data, (const void *)cur->data,
                                 0 | YABO_VTABLE);
    if (status != YABO_STATUS_OK) {
      return {};
    }
    std::swap(cur, next);
  }
  auto slice = reinterpret_cast<Slice *>(cur->data);
  return ByteSpan(reinterpret_cast<const uint8_t *>(slice->start),
                  reinterpret_cast<const uint8_t *>(slice->end));
}

std::optional<ByteSpan> YaboValCreator::extent(YaboVal val) {
  if (val.kind() == YaboValKind::YABOU8) {
    auto start = val.access_u8();
    return ByteSpan(start, 1);
  }
  if (val.kind() == YaboValKind::YABOARRAY) {
    return storage.tmp_buf_o_plenty<std::optional<ByteSpan>>(
        [=](DynValue *t1, DynValue *t2) -> std::optional<ByteSpan> {
          // this cannot fail as we are already at deref level 0
          val->vtable->typecast_impl(t1->data, val->data, 0 | YABO_VTABLE);
          return primary_slice(t1, t2);
        });
  }
  if (val.kind() != YaboValKind::YABONOM) {
    return {};
  }
  auto vtable = reinterpret_cast<const NominalVTable *>(val->vtable);
  return storage.tmp_buf_o_plenty<std::optional<ByteSpan>>(
      [=](DynValue *t1, DynValue *t2, DynValue *t3) -> std::optional<ByteSpan> {
        auto start = t1;
        if (catch_segfault(vtable->start_impl, segfault_err_code,
                           (void *)start->data, (const void *)val->data,
                           0 | YABO_VTABLE)) {
          return {};
        }
        auto end = t2;
        if (catch_segfault(vtable->end_impl, segfault_err_code,
                           (void *)end->data, (const void *)val->data,
                           (uint64_t)0)) {
          return {};
        }
        auto array_vtable =
            reinterpret_cast<const ArrayVTable *>(start->vtable);
        auto extent = t3;
        auto status =
            catch_segfault(array_vtable->span_impl, segfault_err_code,
                           (void *)extent->data, (const void *)start->data,
                           0 | YABO_VTABLE, (const void *)end->data);
        if (status != YABO_STATUS_OK) {
          return {};
        }
        return primary_slice(extent, t2);
      });
}

std::optional<SpannedVal>
YaboValCreator::parse(ParseFun parser, const void *args, ByteSpan buf) {
  return storage.with_span_and_return_buf(buf, [=](void *addr, uint8_t *ret) {
    return catch_segfault(parser, segfault_err_code, (void *)ret, args,
                          DEFAULT_LEVEL | YABO_VTABLE, addr);
  });
}

YaboValCreator
init_vals_from_lib(void *lib,
                   std::pair<const uint8_t *, const uint8_t *> span) {
  auto size = reinterpret_cast<size_t *>(dlsym(lib, "yabo_max_buf_size"));
  if (!size) {
    throw std::runtime_error("Failed to get yabo_max_buf_size from library");
  }

  auto [start, end] = span;

  auto global_init = reinterpret_cast<InitFun>(dlsym(lib, YABO_GLOBAL_INIT));
  int64_t status = global_init(start, end);
  if (status) {
    throw std::runtime_error("Failed to initialize global state");
  }
  return YaboValCreator(YaboValStorage(*size));
}