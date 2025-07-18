#pragma once
#ifdef __cplusplus
extern "C" {
#endif

#include <stdalign.h>
#include <stdint.h>
#include <sys/types.h>

#define YABO_DISC_MASK (~(int64_t)0xff)

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

struct VTableHeader {
  int64_t head;
  int64_t deref_level;
  int64_t (*typecast_impl)(void *, const void *, uint64_t);
  size_t (*mask_impl)(void *);
  size_t size;
  size_t align;
};

struct BlockFields {
  size_t number_fields;
  const char *fields[];
};

struct BlockVTable {
  struct VTableHeader head;
  struct BlockFields *fields;
  int64_t (*access_impl[])(void *, const void *, uint64_t);
};

struct NominalVTable {
  struct VTableHeader head;
  const char *name;
  int64_t (*start_impl)(void *, const void *, uint64_t);
  int64_t (*end_impl)(void *, const void *, uint64_t);
};

typedef int64_t (*ParseFun)(void *, const void *, uint64_t, void *);
typedef int64_t (*LenFun)(int64_t *, const void *);

struct ParserVTable {
  struct VTableHeader head;
  LenFun len_impl;
  ParseFun apply_table[];
};

struct ArrayVTable {
  struct VTableHeader head;
  int64_t (*single_forward_impl)(void *);
  int64_t (*current_element_impl)(void *, const void *, uint64_t);
  int64_t (*array_len_impl)(const void *);
  int64_t (*skip_impl)(void *, uint64_t);
  int64_t (*span_impl)(void *, const void *, uint64_t, const void *);
  int64_t (*inner_array_impl)(void *, const void *, uint64_t);
};

struct ParserExport {
  ParseFun parser;
  const struct VTableHeader *args[];
};

typedef int64_t (*InitFun)(const uint8_t *, const uint8_t *);

#ifdef __cplusplus
}
#endif
