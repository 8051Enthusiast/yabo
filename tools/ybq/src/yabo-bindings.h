#pragma once
#include <stddef.h>
#include <stdint.h>
struct DynValue;

enum YbqReturnStatus {
  YBQ_STATUS_OK = 0,
  YBQ_STATUS_ERROR = 1,
  YBQ_STATUS_EOS = 2,
  YBQ_STATUS_BACKTRACK = 3,
};

enum YbqType {
  YBQ_INT,
  YBQ_CHAR,
  YBQ_BIT,
  YBQ_ERROR,
  YBQ_ARRAY,
  YBQ_BLOCK,
  YBQ_UNIT,
  YBQ_PARSER,
  YBQ_FUNCTION,
};

int64_t ybq_call_init(const uint8_t *begin, size_t len, void *init_fun);

int ybq_type(const struct DynValue *val);

size_t ybq_alloc_size(const struct DynValue *val);

int64_t ybq_parse_bytes_with_args(struct DynValue *ret, const uint8_t *begin, size_t len,
                                  void *parser_export, const void *args);

size_t ybq_export_arg_count(void *export_info);

size_t ybq_field_name_index(const struct DynValue *block, const char *name);

int64_t ybq_field_access(struct DynValue *ret, const struct DynValue *block,
                         size_t index);

size_t ybq_field_count(const struct DynValue *block);

char *ybq_field_name_at_index(const struct DynValue *block, size_t index);

size_t ybq_array_size(const struct DynValue *array);

int64_t ybq_array_access(struct DynValue *ret, struct DynValue *array,
                         size_t idx);

int64_t ybq_int(const struct DynValue *integer);

int32_t ybq_char(const struct DynValue *character);

uint8_t ybq_bit(const struct DynValue *bit);

uint64_t ybq_error(const struct DynValue *val);