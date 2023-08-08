#pragma once
#include <stdint.h>
#include <sys/types.h>

enum YaboHead {
  YABO_INTEGER = 0x100,
  YABO_BIT = 0x200,
  YABO_CHAR = 0x300,
  YABO_LOOP = 0x400,
  YABO_PARSER = 0x500,
  YABO_FUN_ARGS = 0x600,
  YABO_BLOCK = 0x700,
  YABO_ANY = UINT64_MAX & ~0xff,
  YABO_VTABLE = 1,
};

enum ReturnStatus {
  OK = 0,
  ERROR = 1,
  EOS = 2,
  BACKTRACK = 3,
};

struct VTableHeader {
  int64_t head;
  size_t deref_level;
  int64_t (*typecast_impl)(void *, void *, uint64_t);
  size_t (*mask_impl)(void *);
  size_t size;
  size_t align;
};

struct BlockFields {
  size_t number_fields;
  char *fields[];
};

struct BlockVTable {
  struct VTableHeader head;
  struct BlockFields *fields;
  int64_t (*access_impl[])(void *, void *, uint64_t);
};

struct NominalVTable {
  struct VTableHeader head;
  int64_t (*start_impl)(void *, void *);
  int64_t (*end_impl)(void *, void *);
};

typedef int64_t (*ParseFun)(void *, void *, uint64_t, void *);
typedef int64_t (*LenFun)(int64_t *, void *);

struct ParserVTable {
  struct VTableHeader head;
  LenFun len_impl;
  int64_t (*apply_table[])(void *, void *, uint64_t, void *);
};

struct ArrayVTable {
  struct VTableHeader head;
  int64_t (*single_forward_impl)(void *);
  int64_t (*current_element_impl)(void *, void *, uint64_t);
  int64_t (*array_len_impl)(void *);
  int64_t (*skip_impl)(void *, uint64_t);
  int64_t (*span_impl)(void *, void *, uint64_t, void *);
};
