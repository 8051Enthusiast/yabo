#pragma once
#ifdef __cplusplus
extern "C" {
#endif

#include "vtable.h"
#include <stdalign.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

struct Slice {
  const uint8_t *start;
  const uint8_t *end;
};

typedef struct {
  // padding such that the vtable is directly infront of data without
  // padding, and such that data[] has the necessary alignment
  alignas(YABO_MAX_ALIGN) char padding[YABO_MAX_ALIGN - sizeof(void *)];
  struct VTableHeader *vtable;
  char data[];
} DynValue;

// sets the DynValue to be an error value containing the status code
// as a 64 bit integer
static inline int64_t dyn_invalidate(DynValue *val, int64_t status) {
  val->vtable = 0;
  memcpy(val->data, &status, sizeof(int64_t));
  return status;
}

// determines the size of the DynValue, including the vtable
// but not including the padding
static inline size_t dyn_val_size(const DynValue *val) {
  if (!val->vtable) {
    return sizeof(int64_t) + sizeof(struct VTableHeader *);
  }
  return val->vtable->size + sizeof(struct VTableHeader *);
}

// calls the parser with the given bytes, and stores the result in ret
static inline int64_t dyn_parse_bytes(DynValue *ret, struct Slice bytes,
                                      ParseFun parser) {
  int64_t status = parser(ret->data, NULL, YABO_ANY | YABO_VTABLE, &bytes);
  if (status) {
    return dyn_invalidate(ret, status);
  }
  return status;
}

// dereferences val for one level, and stores the result in ret
static inline int64_t dyn_deref(DynValue *ret, const DynValue *val) {
  const struct NominalVTable *vtable =
      (const struct NominalVTable *)val->vtable;
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

// copies the value of val into ret
static inline void dyn_copy(DynValue *ret, const DynValue *val) {
  size_t size = dyn_val_size(val);
  // start copying from the vtable, skipping the padding
  memcpy(&ret->vtable, &val->vtable, size);
}

// finds the index of the field with the given name of the block
// returns -1 if the field is not found
static inline size_t dyn_field_name_index(const DynValue *block,
                                          const char *name) {
  const struct BlockVTable *vtable = (const struct BlockVTable *)block->vtable;

  const char **result = (const char **)bsearch(
      &name, vtable->fields->fields, vtable->fields->number_fields,
      sizeof(char *), (int (*)(const void *, const void *))strcmp);

  if (!result) {
    return -1;
  }

  return result - vtable->fields->fields;
}

// accesses the field of the block at the given index, and stores the result in
// ret
static inline int64_t
dyn_access_field_index(DynValue *ret, const DynValue *block, size_t index) {
  const struct BlockVTable *vtable = (const struct BlockVTable *)block->vtable;
  if (index >= vtable->fields->number_fields) {
    return dyn_invalidate(ret, YABO_STATUS_ERROR);
  }
  int64_t status = vtable->access_impl[index](ret->data, block->data,
                                              YABO_ANY | YABO_VTABLE);
  if (status) {
    return dyn_invalidate(ret, status);
  }
  return status;
}

// returns the number of fields in the block
static inline size_t dyn_block_field_count(const DynValue *block) {
  const struct BlockVTable *vtable = (const struct BlockVTable *)block->vtable;
  return vtable->fields->number_fields;
}

// returns the name of the field at the given index, or NULL if the index
// is out of bounds
static inline const char *dyn_block_field_name_at_index(const DynValue *block,
                                                        size_t index) {
  const struct BlockVTable *vtable = (const struct BlockVTable *)block->vtable;
  if (index >= vtable->fields->number_fields) {
    return NULL;
  }
  return vtable->fields->fields[index];
}

// modifies `array` to point to the next element in the array
static inline int64_t dyn_array_single_forward(DynValue *array) {
  const struct ArrayVTable *vtable = (const struct ArrayVTable *)array->vtable;
  uint64_t status = vtable->single_forward_impl(array->data);
  if (status) {
    return dyn_invalidate(array, status);
  }
  return status;
}

// gets the current element of the array, storing it in ret
static inline int64_t dyn_array_current_element(DynValue *ret,
                                                const DynValue *array) {
  const struct ArrayVTable *vtable = (const struct ArrayVTable *)array->vtable;
  uint64_t status = vtable->current_element_impl(ret->data, array->data,
                                                 YABO_ANY | YABO_VTABLE);
  if (status) {
    return dyn_invalidate(ret, status);
  }
  return status;
}

// gets the length of `array`
static inline int64_t dyn_array_len(const DynValue *array) {
  const struct ArrayVTable *vtable = (const struct ArrayVTable *)array->vtable;
  return vtable->array_len_impl(array->data);
}

static inline int64_t dyn_int(const DynValue *integer) {
  int64_t ret;
  memcpy(&ret, integer->data, sizeof(ret));
  return ret;
}

static inline int32_t dyn_char(const DynValue *chr) {
  int32_t ret;
  memcpy(&ret, chr->data, sizeof(ret));
  return ret;
}

static inline int8_t dyn_bit(const DynValue *bit) {
  int8_t ret;
  memcpy(&ret, bit->data, sizeof(ret));
  return ret;
}

static inline const uint8_t *dyn_u8(const DynValue *u8) {
  const uint8_t *ret;
  memcpy(&ret, u8->data, sizeof(ret));
  return ret;
}

static inline uint64_t dyn_error(const DynValue *error) {
  uint64_t ret;
  memcpy(&ret, error->data, sizeof(ret));
  return ret;
}

#ifdef __cplusplus
}
#endif
