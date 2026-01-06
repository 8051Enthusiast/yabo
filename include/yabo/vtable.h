#pragma once
#ifdef __cplusplus
extern "C" {
#endif

#include <stdalign.h>
#include <stdint.h>
#include <stddef.h>

#define YABO_DISC_MASK (~(int64_t)0xff)

#ifndef YABO_RELATIVE_VPTR
#if __EMSCRIPTEN__ || __wasi__
#define YABO_RELATIVE_VPTR 0
#else
#define YABO_RELATIVE_VPTR 1
#endif
#endif

#if YABO_RELATIVE_VPTR

#define YABO_VPTR(x) struct { int32_t offset; __attribute__((packed)) x* phantom[0]; }
#define YABO_ACCESS_VPTR(val, field) (!(val)->field.offset ? NULL : (typeof((val)->field.phantom[0]))((char*)(val) + (val)->field.offset))

#else

#define YABO_VPTR(x) x*
#define YABO_ACCESS_VPTR(val, field) (val)->field

#endif

enum YaboHead {
  YABO_INTEGER = 0x100,
  YABO_BIT = 0x200,
  YABO_CHAR = 0x300,
  YABO_LOOP = 0x400,
  YABO_SLICEPTR = 0x401,
  YABO_PARSER = 0x500,
  YABO_FUN_ARGS = 0x600,
  YABO_BLOCK = 0x700,
  YABO_UNIT = 0x800,
  YABO_U8 = 0x900,
  YABO_ANY = UINT64_MAX & YABO_DISC_MASK,
  YABO_VTABLE = 1,
};

#define YABO_MAX_ALIGN (((alignof(int64_t) - 1) | (alignof(void *) - 1)) + 1)

enum ReturnStatus {
  YABO_STATUS_OK = 0,
  YABO_STATUS_ERROR = 1,
  YABO_STATUS_EOS = 2,
  YABO_STATUS_BACKTRACK = 3,
};

typedef int64_t TypecastFun(void *, const void *, uint64_t);
typedef size_t MaskFun(void *);

struct VTableHeader {
  int64_t head;
  int64_t deref_level;
  YABO_VPTR(TypecastFun) typecast_impl;
  YABO_VPTR(MaskFun) mask_impl;
  size_t size;
  size_t align;
};

struct BlockFields {
  size_t number_fields;
  YABO_VPTR(const char) fields[];
};

typedef int64_t AccessFun(void *, const void *, uint64_t);

struct BlockVTable {
  struct VTableHeader head;
  YABO_VPTR(struct BlockFields) fields;
  YABO_VPTR(AccessFun) access_impl[];
};

typedef int64_t PositionFun(void *, const void *, uint64_t);

struct NominalVTable {
  struct VTableHeader head;
  YABO_VPTR(const char) name;
  YABO_VPTR(PositionFun) start_impl;
  YABO_VPTR(PositionFun) end_impl;
};

typedef int64_t ParseFun(void *, const void *, uint64_t, void *);
typedef int64_t LenFun(int64_t *, const void *);

struct ParserVTable {
  struct VTableHeader head;
  YABO_VPTR(LenFun) len_impl;
  YABO_VPTR(ParseFun) apply_table[];
};

typedef int64_t SingleForwardFun(void *);
typedef int64_t CurrentElementFun(void *, const void *, uint64_t);
typedef int64_t ArrayLenFun(const void *);
typedef int64_t SkipFun(void *, uint64_t);
typedef int64_t SpanFun(void *, const void *, uint64_t, const void *);
typedef int64_t InnerArrayFun(void *, const void *, uint64_t);

struct ArrayVTable {
  struct VTableHeader head;
  YABO_VPTR(SingleForwardFun) single_forward_impl;
  YABO_VPTR(CurrentElementFun) current_element_impl;
  YABO_VPTR(ArrayLenFun) array_len_impl;
  YABO_VPTR(SkipFun) skip_impl;
  YABO_VPTR(SpanFun) span_impl;
  YABO_VPTR(InnerArrayFun) inner_array_impl;
};

struct ParserExport {
  YABO_VPTR(ParseFun) parser;
  YABO_VPTR(const struct VTableHeader) args[];
};

typedef int64_t InitFun(const uint8_t *, const uint8_t *);

#undef YABO_VPTR

#ifdef __cplusplus
}
#endif