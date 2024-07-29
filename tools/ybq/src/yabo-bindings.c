#include "yabo-bindings.h"
#include <yabo/dynamic.h>
#include <yabo/vtable.h>

int64_t ybq_call_init(const uint8_t *begin, size_t len, void *init_fun) {
  return ((InitFun)init_fun)(begin, begin + len);
}

int ybq_type(const struct DynValue *val) {
  if (!val->vtable) {
    return YBQ_ERROR;
  }
  switch (val->vtable->head & YABO_DISC_MASK) {
  case YABO_INTEGER:
    return YBQ_INT;
  case YABO_CHAR:
    return YBQ_CHAR;
  case YABO_BIT:
    return YBQ_BIT;
  case YABO_BLOCK:
    return YBQ_BLOCK;
  case YABO_UNIT:
    return YBQ_UNIT;
  case YABO_PARSER:
    return YBQ_PARSER;
  case YABO_FUN_ARGS:
    return YBQ_FUNCTION;
  case YABO_LOOP:
    return YBQ_ARRAY;
  default:
    return YBQ_ERROR;
  }
}

size_t ybq_alloc_size(const struct DynValue *val) {
  if (!val->vtable) {
    return sizeof(int64_t) + offsetof(DynValue, data);
  }
  return val->vtable->size + offsetof(DynValue, data);
}

int64_t ybq_parse_bytes(struct DynValue *ret, const uint8_t *begin, size_t len,
                        void *parser) {
  struct Slice bytes = {begin, begin + len};
  int64_t status = ((ParseFun)parser)(ret->data, NULL, YABO_VTABLE, &bytes);
  if (status) {
    return dyn_invalidate(ret, status);
  }
  return status;
}

size_t ybq_field_name_index(const struct DynValue *block, const char *name) {
  return dyn_field_name_index(block, name);
}

int64_t ybq_field_access(struct DynValue *ret, const struct DynValue *block,
                         size_t index) {
  const struct BlockVTable *vtable = (const struct BlockVTable *)block->vtable;
  int64_t status =
      vtable->access_impl[index](ret->data, block->data, YABO_VTABLE);
  if (status) {
    return dyn_invalidate(ret, status);
  }
  return status;
}

size_t ybq_field_count(const struct DynValue *block) {
  return dyn_block_field_count(block);
}

char *ybq_field_name_at_index(const struct DynValue *block, size_t index) {
  return (char *)dyn_block_field_name_at_index(block, index);
}

size_t ybq_array_size(const struct DynValue *array) {
  return dyn_array_len(array);
}

int64_t ybq_array_access(struct DynValue *ret, struct DynValue *array,
                         size_t idx) {
  uint64_t status = dyn_array_skip(array, idx);
  if (status) {
    return dyn_invalidate(ret, status);
  }
  const struct ArrayVTable *vtable = (const struct ArrayVTable *)array->vtable;
  status = vtable->current_element_impl(ret->data, array->data, YABO_VTABLE);
  if (status) {
    return dyn_invalidate(ret, status);
  }
  return status;
}

int64_t ybq_int(const struct DynValue *integer) { return dyn_int(integer); }

int32_t ybq_char(const struct DynValue *character) {
  return dyn_char(character);
}

uint8_t ybq_bit(const struct DynValue *bit) { return dyn_bit(bit); }

uint64_t ybq_error(const struct DynValue *val) { return dyn_error(val); }
