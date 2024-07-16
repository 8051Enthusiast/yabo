#include <dlfcn.h>
#include <inttypes.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include <yabo/dynamic.h>
#include <yabo/vtable.h>

typedef struct {
  DynValue *current;
  char *limit;
} Stack;

// 16 MB stack
#define STACK_SIZE 1024 * 1024 * 16

Stack init_stack(size_t max_dyn_size) {
  Stack stack;
  if (max_dyn_size > STACK_SIZE) {
    fprintf(stderr, "Max dyn size too large\n");
    exit(1);
  }
  stack.current = malloc(STACK_SIZE);
  if (!stack.current) {
    fprintf(stderr, "Could not allocate stack\n");
    exit(1);
  }
  stack.limit = (char *)stack.current + STACK_SIZE;
  stack.limit -= max_dyn_size;
  return stack;
}

void free_stack(Stack stack) { free(stack.current); }

Stack bump(Stack stack) {
  size_t size = dyn_val_size(stack.current);
  size_t aligned_size =
      (size + alignof(DynValue) - 1) & ~(alignof(DynValue) - 1);
  if ((size_t)(stack.limit - (char *)stack.current) < aligned_size) {
    fprintf(stderr, "Value stack overflow\n");
    exit(1);
  }
  stack.current = (DynValue *)((char *)stack.current + aligned_size);
  return stack;
}

#define fputc_ret(chr, file)                                                   \
  {                                                                            \
    if (fputc(chr, file) == EOF)                                               \
      return EOF;                                                              \
  }

static inline int print_indent(int indent, FILE *out) {
  for (int i = 0; i < indent; i++) {
    fputc_ret(' ', out);
  }
  return 0;
}

int print_recursive(int indent, Stack stack, FILE *out);

int print_char(DynValue *val, int indent, Stack stack, FILE *out) {
  int32_t char_value = dyn_char(val);
  fputc_ret('"', out);
  if (char_value < 0x80) {
    fputc_ret(char_value, out);
  } else if (char_value < 0x800) {
    fputc_ret(0xc0 | char_value >> 6, out);
    fputc_ret(0x80 | (0x3f & char_value), out);
  } else if (char_value < 0x10000) {
    fputc_ret(0xe0 | char_value >> 12, out);
    fputc_ret(0x80 | (0x3f & (char_value >> 6)), out);
    fputc_ret(0x80 | (0x3f & char_value), out);
  } else {
    fputc_ret(0xf0 | char_value >> 18, out);
    fputc_ret(0x80 | (0x3f & (char_value >> 12)), out);
    fputc_ret(0x80 | (0x3f & (char_value >> 6)), out);
    fputc_ret(0x80 | (0x3f & char_value), out);
  }
  fputc_ret('"', out);
  return 0;
}

int print_int(DynValue *val, int indent, Stack stack, FILE *out) {
  int64_t int_value = dyn_int(val);
  return fprintf(out, "%" PRId64, int_value);
}

int print_bit(DynValue *val, int indent, Stack stack, FILE *out) {
  int8_t bit = dyn_bit(val);
  const char *text;
  if (bit) {
    text = "true";
  } else {
    text = "false";
  }
  return fputs(text, out);
}

int print_parser(DynValue *val, int indent, Stack stack, FILE *out) {
  struct ParserVTable *vtable = (struct ParserVTable *)val->vtable;
  int64_t len;
  int64_t ret = vtable->len_impl(&len, val->data);
  if (ret != YABO_STATUS_OK) {
    return fputs("\"parser\"", out);
  } else {
    return fprintf(out, "\"parser(%" PRId64 ")\"", len);
  }
}

int print_fun_args(DynValue *val, int indent, Stack stack, FILE *out) {
  // not really much we can print
  return fputs("\"fun_args\"", out);
}

int print_block(DynValue *val, int indent, Stack stack, FILE *out) {
  struct BlockVTable *vtable = (struct BlockVTable *)val->vtable;
  char **field_desc = vtable->fields->fields;
  char **field_end = field_desc + vtable->fields->number_fields;
  if (fputs("{\n", out) == EOF)
    return EOF;
  int64_t (**access_impl)(void *, const void *, uint64_t) = vtable->access_impl;
  int first = 1;
  while (field_desc != field_end) {
    DynValue *sub_value = (DynValue *)stack.current;
    int64_t return_val =
        (*access_impl)(sub_value->data, val->data, YABO_VTABLE);
    if (return_val == 3) {
      access_impl++;
      field_desc++;
      continue;
    }
    if (return_val != 0) {
      return -1;
    }
    if (!first) {
      if (fputs(",\n", out) == EOF) {
        return EOF;
      }
    }
    first = 0;
    if (print_indent(indent + 2, out) == EOF)
      return EOF;
    if (fprintf(out, "\"%s\": ", *field_desc) < 0)
      return EOF;
    if (print_recursive(indent + 2, stack, out) < 0)
      return EOF;
    access_impl++;
    field_desc++;
  }
  if (fputs("\n", out) == EOF) {
    return EOF;
  }
  if (print_indent(indent, out) == EOF)
    return EOF;
  fputc_ret('}', out);
  return 0;
}

int print_array(DynValue *val, int indent, Stack stack, FILE *out) {
  int64_t len = dyn_array_len(val);
  if (fputs("[\n", out) == EOF)
    return EOF;
  for (int64_t i = 0; i < len; i++) {
    DynValue *sub_value = stack.current;
    dyn_array_current_element(sub_value, val);
    if (i) {
      if (fputs(",\n", out) == EOF) {
        return EOF;
      }
    }
    if (print_indent(indent + 2, out) == EOF)
      return EOF;
    if (print_recursive(indent + 2, stack, out) < 0)
      return EOF;
    dyn_array_single_forward(val);
  }
  if (fputs("\n", out) == EOF) {
    return EOF;
  }
  if (print_indent(indent, out) == EOF)
    return EOF;
  fputc_ret(']', out);
  return 0;
}

int print_indirect(DynValue *val, int indent, Stack stack, FILE *out) {
  DynValue *deref = stack.current;
  dyn_deref(deref, val);
  return print_recursive(indent, stack, out);
}

int print_recursive(int indent, Stack stack, FILE *out) {
  int status;
  DynValue *val = stack.current;
  if (!val->vtable) {
    return fputs("null", out);
  }
  struct VTableHeader *vtable = val->vtable;
  vtable->mask_impl(val->data);
  int64_t head = vtable->head & YABO_DISC_MASK;
  Stack substack = bump(stack);
  if (head < 0 || head == YABO_U8) {
    status = print_indirect(val, indent, substack, out);
  } else {
    switch (head) {
    case YABO_INTEGER:
      status = print_int(val, indent, substack, out);
      break;
    case YABO_BIT:
      status = print_bit(val, indent, substack, out);
      break;
    case YABO_CHAR:
      status = print_char(val, indent, substack, out);
      break;
    case YABO_LOOP:
      status = print_array(val, indent, substack, out);
      break;
    case YABO_PARSER:
      status = print_parser(val, indent, substack, out);
      break;
    case YABO_FUN_ARGS:
      status = print_fun_args(val, indent, substack, out);
      break;
    case YABO_BLOCK:
      status = print_block(val, indent, substack, out);
      break;
    case YABO_UNIT:
      status = fputs("\"unit\"", out);
      break;
    }
  }
  return status;
}

struct Slice map_file(char *filename) {
  FILE *f = fopen(filename, "r");
  if (!f) {
    perror("could not open file");
    return (struct Slice){0};
  }
  if (fseek(f, 0, SEEK_END)) {
    perror("could not get file size");
    return (struct Slice){0};
  }
  long length = ftell(f);
  if (length < 0) {
    perror("could not get file size");
    return (struct Slice){0};
  }
  int fd = fileno(f);
  if (fd < 0) {
    perror("could not mmap file");
    return (struct Slice){0};
  }
  const void *file = mmap(NULL, (size_t)length, PROT_READ, MAP_SHARED, fd, 0);
  if (file == MAP_FAILED) {
    perror("could not mmap file");
    return (struct Slice){0};
  }
  return (struct Slice){(const uint8_t *)file, (const uint8_t *)file + length};
}

struct LibInfo {
  size_t max_dyn_size;
  InitFun global_init;
  ParseFun *parser;
};

#ifndef STATIC_PARSER

struct LibInfo dynamic_lib(char *filename, char *parser_name) {
  struct LibInfo ret;
  void *lib = dlopen(filename, RTLD_NOW);
  if (!lib) {
    fprintf(stderr, "could not open library: %s", dlerror());
    exit(1);
  }
  size_t *max_dyn_size_ptr = (size_t *)dlsym(lib, "yabo_max_buf_size");
  if (!max_dyn_size_ptr) {
    perror("could not find yabo_max_buf_size (is this a yabo library?)");
    exit(1);
  }
  ret.max_dyn_size = *max_dyn_size_ptr;
  ret.global_init = dlsym(lib, "yabo_global_init");
  ret.parser = (ParseFun *)dlsym(lib, parser_name);
  return ret;
}

#else

struct LibInfo static_lib() {
  struct LibInfo ret;
  extern size_t yabo_max_buf_size;
  __attribute__((weak)) extern struct Slice yabo_global_address;
  extern int64_t yabo_global_init(const uint8_t *, const uint8_t *);
  extern ParseFun STATIC_PARSER;
  ret.max_dyn_size = yabo_max_buf_size;
  ret.global_init = yabo_global_init;
  ret.parser = &STATIC_PARSER;
  return ret;
}

#endif

int main(int argc, char *argv[argc]) {

#ifndef STATIC_PARSER

  if (argc != 4) {
    fprintf(stderr, "usage: %s SOFILE PARSERNAME FILE\n", argv[0]);
    exit(1);
  }
  struct LibInfo lib = dynamic_lib(argv[1], argv[2]);

#else

  if (argc != 2) {
    fprintf(stderr, "usage: %s FILE\n", argv[0]);
    exit(1);
  }

  struct LibInfo lib = static_lib();

#endif

  struct Slice file = map_file(argv[argc - 1]);
  if (!file.start) {
    exit(1);
  }

  if (!lib.parser) {
    perror("could not find parser");
    exit(1);
  }
  if (lib.global_init) {
    int64_t status = lib.global_init(file.start, file.end);
    if (status != 0) {
      fprintf(stderr,
              "failed to initialize yabo library with status %" PRId64 "\n",
              status);
      exit(1);
    }
  }
  Stack stack = init_stack(lib.max_dyn_size);
  dyn_parse_bytes(stack.current, file, *lib.parser);
  print_recursive(0, stack, stdout);
  free_stack(stack);
  putchar('\n');
}
