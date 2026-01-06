#pragma once
#ifdef __cplusplus
extern "C" {
#endif

#include "vtable.h"
#include <errno.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

static size_t yabo_export_identifier_end(const char *identifier) {
  const char *end = identifier;
  while ((*end >= 'a' && *end <= 'z') || (*end >= 'A' && *end <= 'Z') ||
         (*end >= '0' && *end <= '9') || *end == '_') {
    end++;
  }
  return end - identifier;
}

static size_t yabo_export_args_size(const struct ParserExport *export_info) {
  size_t size = 0;
  size_t index = 0;
  while (1) {
    const struct VTableHeader* vtable = YABO_ACCESS_VPTR(export_info, args[index]);
    if (!vtable) {
      break;
    }

    if (vtable->head != YABO_INTEGER) {
      return -1;
    }
    size += sizeof(int64_t);
    index += 1;
  }
  return size;
}

#define WS(x)                                                                  \
  while (*(x) == ' ' || *(x) == '\t')                                          \
    (x)++;

enum YaboArgParseError {
  YABO_EXPORT_SUCCESS = 0,
  YABO_EXPORT_NOT_ENOUGH_ARGS = 1,
  YABO_EXPORT_TOO_MANY_ARGS = 2,
  YABO_EXPORT_INVALID_CHARACTER = 3,
  YABO_EXPORT_UNSUPPORTED_TYPE = 4,
  YABO_EXPORT_ARGUMENT_OUT_OF_RANGE = 5,
};

static inline const char *yabo_export_parse_error_message(enum YaboArgParseError error) {
  switch (error) {
  case YABO_EXPORT_SUCCESS:
    return "success";
  case YABO_EXPORT_NOT_ENOUGH_ARGS:
    return "not enough arguments provided";
  case YABO_EXPORT_TOO_MANY_ARGS:
    return "too many arguments provided";
  case YABO_EXPORT_INVALID_CHARACTER:
    return "invalid character";
  case YABO_EXPORT_UNSUPPORTED_TYPE:
    return "unsupported type";
  case YABO_EXPORT_ARGUMENT_OUT_OF_RANGE:
    return "argument out of range";
  default:
    return "unknown error";
  }
}

static enum YaboArgParseError yabo_export_parse_arg(const char *args,
                                               const struct ParserExport *export_info,
                                               char *out_buffer) {
  size_t index = 0;
  WS(args);
  switch (*args) {
  case '\0':
    if (YABO_ACCESS_VPTR(export_info, args[index])) {
      return YABO_EXPORT_NOT_ENOUGH_ARGS;
    } else {
      return YABO_EXPORT_SUCCESS;
    }
  case '(':
    args++;
    break;
  default:
    return YABO_EXPORT_INVALID_CHARACTER;
  }
  WS(args);

  while (1) {
    const struct VTableHeader *vtable = YABO_ACCESS_VPTR(export_info, args[index]);
    if (!vtable) {
      break;
    }
    index++;

    int64_t res = 0;
    char *end = NULL;
    if (vtable->head != YABO_INTEGER) {
      return YABO_EXPORT_UNSUPPORTED_TYPE;
    }

    errno = 0;
    if (args[0] == '0' && (args[1] == 'x' || args[1] == 'X')) {
      args += 2;
      res = strtoll(args, &end, 16);
    } else {
      res = strtoll(args, &end, 10);
    }

    if (args == end) {
      return YABO_EXPORT_INVALID_CHARACTER;
    }

    if (errno == ERANGE) {
      return YABO_EXPORT_ARGUMENT_OUT_OF_RANGE;
    }

    args = end;
    memcpy(out_buffer, &res, sizeof(int64_t));
    out_buffer += sizeof(int64_t);

    WS(args);

    if (!YABO_ACCESS_VPTR(export_info, args[index])) {
      break;
    }

    if (*args != ',') {
      return YABO_EXPORT_INVALID_CHARACTER;
    }
    args++;
    WS(args);
  }

  if (*args != ')') {
    return YABO_EXPORT_TOO_MANY_ARGS;
  }
  args++;
  WS(args);

  if (*args) {
    return YABO_EXPORT_INVALID_CHARACTER;
  }

  return YABO_EXPORT_SUCCESS;
}

#undef WS

#ifdef __cplusplus
}
#endif