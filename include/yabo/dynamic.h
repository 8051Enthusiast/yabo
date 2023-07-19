#pragma once
#include "vtable.h"
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

struct Slice {
  void *start;
  void *end;
};

typedef struct {
  struct VTableHeader *vtable;
  char data[];
} DynValue;

static inline int64_t dyn_invalidate(DynValue *val, int64_t status) {
  val->vtable = 0;
  *((int64_t *)val->data) = status;
  return status;
}

static inline int64_t dyn_parse_bytes(DynValue *ret, struct Slice bytes,
                                      ParseFun parser) {
  int64_t status = parser(ret->data, NULL, YABO_ANY | YABO_VTABLE, &bytes);
  if (status != 0) {
    return dyn_invalidate(ret, status);
  }
  return status;
}

static inline int64_t dyn_deref(DynValue *ret, DynValue *val) {
  struct NominalVTable *vtable = (struct NominalVTable *)val->vtable;
  uint64_t level = vtable->head.deref_level;
  if (level > 0) {
    level -= 256;
  }
  int64_t status =
      vtable->head.typecast_impl(ret->data, val->data, level | YABO_VTABLE);
  if (status) {
    return dyn_invalidate(ret, status);
  }
  return status;
}

static inline void dyn_copy(DynValue *ret, DynValue *val) {
  size_t size = val->vtable->size;
  memcpy(ret, val, size + sizeof(struct VTableHeader *));
}

static inline int64_t dyn_access_field(DynValue *ret, DynValue *block,
                                       char *name) {
  struct BlockVTable *vtable = (struct BlockVTable *)block->vtable;
  char **start = vtable->fields->fields;
  int64_t (*access_impl)(void *, void *, uint64_t) = NULL;
  for (size_t i = 0; i < vtable->fields->number_fields; i++) {
    char *current_name = start[i];
    if (!strcmp(current_name, name)) {
      access_impl = vtable->access_impl[i];
      break;
    }
  }
  if (!access_impl) {
    return dyn_invalidate(ret, ERROR);
  }
  int64_t status = access_impl(ret->data, block->data, YABO_ANY | YABO_VTABLE);
  if (status) {
    return dyn_invalidate(ret, status);
  }
  return status;
}

static inline int64_t dyn_array_single_forward(DynValue *array) {
  struct ArrayVTable *vtable = (struct ArrayVTable *)array->vtable;
  uint64_t status = vtable->single_forward_impl(array->data);
  if (status) {
    return dyn_invalidate(array, status);
  }
  return status;
}

static inline int64_t dyn_array_current_element(DynValue *ret,
                                                DynValue *array) {
  struct ArrayVTable *vtable = (struct ArrayVTable *)array->vtable;
  uint64_t status = vtable->current_element_impl(ret->data, array->data,
                                                 YABO_ANY | YABO_VTABLE);
  if (status) {
    return dyn_invalidate(ret, status);
  }
  return status;
}

static inline int64_t dyn_array_len(DynValue *array) {
  struct ArrayVTable *vtable = (struct ArrayVTable *)array->vtable;
  return vtable->array_len_impl(array->data);
}

static inline int64_t dyn_int(DynValue *integer) {
  return *(int64_t *)integer->data;
}

static inline int32_t dyn_char(DynValue *chr) { return *(int32_t *)chr->data; }

static inline int8_t dyn_bit(DynValue *bit) { return *(int8_t *)bit->data; }
