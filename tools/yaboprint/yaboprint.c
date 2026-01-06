#include <dlfcn.h>
#include <inttypes.h>
#include <stdio.h>
#include <sys/mman.h>
#include <unistd.h>
#include <yabo/dynamic.h>
#include <yabo/parse_export_call.h>
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
  int64_t ret = YABO_ACCESS_VPTR(vtable, len_impl)(&len, val->data);
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
  size_t count = dyn_block_field_count(val);
  if (fputs("{\n", out) == EOF)
    return EOF;

  int first = 1;
  for (size_t i = 0; i < count; i++) {
    DynValue *sub_value = (DynValue *)stack.current;
    int64_t return_val = dyn_access_field_index(sub_value, val, i);
    if (return_val == 3) {
      continue;
    }
    if (return_val != 0) {
      return -1;
    }

    const char *field_desc = dyn_block_field_name_at_index(val, i);

    if (!first) {
      if (fputs(",\n", out) == EOF) {
        return EOF;
      }
    }

    if (print_indent(indent + 2, out) == EOF)
      return EOF;
    if (fprintf(out, "\"%s\": ", field_desc) < 0)
      return EOF;
    if (print_recursive(indent + 2, stack, out) < 0)
      return EOF;

    first = 0;
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
  dyn_mask(val);
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
  InitFun *global_init;
  const struct ParserExport *parser;
  const void *args;
};

const struct ParserExport *get_export(void *lib, char *parser_desc) {
  size_t end = yabo_export_identifier_end(parser_desc);
  char old = parser_desc[end];
  parser_desc[end] = '\0';
  const struct ParserExport *export_info = dlsym(lib, parser_desc);
  parser_desc[end] = old;
  return export_info;
}

const void *export_args(const struct ParserExport *export_info,
                        char *parser_desc) {
  size_t size = yabo_export_args_size(export_info);
  if (size == -1) {
    fprintf(stderr, "unsupported export argument\n");
    exit(1);
  }

  void *args = malloc(size);
  if (!args) {
    fprintf(stderr, "Could not allocate args");
    exit(1);
  }

  void *parser_args = parser_desc + yabo_export_identifier_end(parser_desc);
  enum YaboArgParseError err =
      yabo_export_parse_arg(parser_args, export_info, args);
  if (err) {
    fprintf(stderr, "Could not parse args: %s\n",
            yabo_export_parse_error_message(err));
    exit(1);
  }
  return args;
}

#if defined(STATIC_PARSER)

struct LibInfo static_lib() {
  struct LibInfo ret;
  extern size_t yabo_max_buf_size;
  __attribute__((weak)) extern struct Slice yabo_global_address;
  extern int64_t yabo_global_init(const uint8_t *, const uint8_t *);
  extern struct ParserExport STATIC_PARSER;
  ret.max_dyn_size = yabo_max_buf_size;
  ret.global_init = yabo_global_init;
  ret.parser = &STATIC_PARSER;
  ret.args = NULL;
  return ret;
}

#elif defined(ELF_INTERP)

#include <elf.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/auxv.h>

struct DynInfo {
  uint32_t *gnu_hash;
  char *strtab;
  char *symtab;
  size_t syment;
  size_t offset;
};

int find_dyn_info(struct DynInfo *out, Elf64_Phdr *dynamic_phdr,
                  size_t offset) {
  *out = (struct DynInfo){0};
  size_t dyn_count = dynamic_phdr->p_memsz / sizeof(Elf64_Dyn);
  Elf64_Dyn *dyn_addr = (Elf64_Dyn *)(dynamic_phdr->p_vaddr + offset);
  for (size_t i = 0; i < dyn_count; i++) {
    switch (dyn_addr[i].d_tag) {
    case DT_STRTAB:
      out->strtab = (char *)(dyn_addr[i].d_un.d_ptr + offset);
      break;
    case DT_SYMTAB:
      out->symtab = (char *)(dyn_addr[i].d_un.d_ptr + offset);
      break;
    case DT_SYMENT:
      out->syment = dyn_addr[i].d_un.d_val;
      break;
    case DT_GNU_HASH:
      out->gnu_hash = (uint32_t *)(dyn_addr[i].d_un.d_ptr + offset);
      break;
    }
  }

  return out->gnu_hash && out->strtab && out->symtab && out->syment;
}

uint32_t djb2(char *str) {
  uint32_t hash = 5381;
  unsigned char c;

  while ((c = (unsigned char)*str++)) {
    hash = 33 * hash + c;
  }

  return hash;
}

char *get_sym(struct DynInfo *info, uint32_t idx) {
  uint32_t stridx = *(uint32_t *)(info->symtab + info->syment * idx);
  return info->strtab + stridx;
}

void *lookup_gnu_hash(struct DynInfo *info, char *name, size_t offset) {
  uint32_t nbuckets = info->gnu_hash[0];
  uint32_t symoffset = info->gnu_hash[1];
  uint32_t bloom_size = info->gnu_hash[2];
  uint32_t *bloom = info->gnu_hash + 4;
  uint32_t *buckets = bloom + bloom_size * 2;
  uint32_t *chain = buckets + nbuckets;
  uint32_t sym_hash = djb2(name);

  uint32_t idx = buckets[sym_hash % nbuckets];
  if (idx < symoffset) {
    return NULL;
  }

  uint32_t *cur_hash = chain + (idx - symoffset);
  do {
    if ((*cur_hash ^ sym_hash) > 1) {
      continue;
    }

    Elf64_Sym *sym = (Elf64_Sym *)(info->symtab + info->syment * idx);
    if (!strcmp(info->strtab + sym->st_name, name)) {
      return (void *)(sym->st_value + offset);
    }
  } while (idx++, !(*cur_hash++ & 1));
  return NULL;
}

struct LibInfo exec_lib() {
  size_t phdr = (size_t)getauxval(AT_PHDR);
  size_t size = (size_t)getauxval(AT_PHENT);
  size_t count = (size_t)getauxval(AT_PHNUM);
  Elf64_Phdr *phdr_phdr = NULL;
  Elf64_Phdr *dynamic_phdr = NULL;
  for (size_t i = 0; i < count; i++) {
    Elf64_Phdr *cur = (Elf64_Phdr *)(phdr + size * i);
    if (cur->p_type == PT_PHDR)
      phdr_phdr = cur;
    else if (cur->p_type == PT_DYNAMIC)
      dynamic_phdr = cur;
  }

  if (!phdr_phdr) {
    fprintf(stderr, "could not find phdr for phdr\n");
    exit(1);
  }

  if (!dynamic_phdr) {
    fprintf(stderr, "could not find the dynamic segment\n");
    exit(1);
  }

  ptrdiff_t offset = phdr - phdr_phdr->p_vaddr;

  struct DynInfo info;
  if (!find_dyn_info(&info, dynamic_phdr, offset)) {
    fprintf(stderr, "could not find dynamic info\n");
    exit(1);
  }

  struct LibInfo lib;
  lib.global_init = lookup_gnu_hash(&info, "yabo_global_init", offset);
  lib.parser = lookup_gnu_hash(&info, "main", offset);
  size_t *yabo_max_buf_size =
      lookup_gnu_hash(&info, "yabo_max_buf_size", offset);
  if (!lib.parser) {
    fprintf(stderr, "could not find main function\n");
    exit(1);
  }
  if (!yabo_max_buf_size) {
    fprintf(stderr,
            "could not find yabo_max_buf_size (is this a yabo library?)\n");
    exit(1);
  }
  lib.max_dyn_size = *yabo_max_buf_size;
  lib.args = NULL;
  return lib;
}

#else

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
  ret.parser = get_export(lib, parser_name);
  if (ret.parser) {
    ret.args = export_args(ret.parser, parser_name);
  } else {
    ret.args = NULL;
  }
  return ret;
}

#endif

int main(int argc, char *argv[argc]) {

#if defined(STATIC_PARSER) || defined(ELF_INTERP)

  if (argc != 2) {
    fprintf(stderr, "usage: %s FILE\n", argv[0]);
    exit(1);
  }

#ifdef STATIC_PARSER
  struct LibInfo lib = static_lib();
#else
  struct LibInfo lib = exec_lib();
#endif

#else

  if (argc != 4) {
    fprintf(stderr, "usage: %s SOFILE PARSERNAME FILE\n", argv[0]);
    exit(1);
  }
  struct LibInfo lib = dynamic_lib(argv[1], argv[2]);

  if (!lib.parser) {
    fprintf(stderr, "could not find parser: %s\n", dlerror());
    exit(1);
  }

#endif

  struct Slice file = map_file(argv[argc - 1]);
  if (!file.start) {
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

  ParseFun *parse = YABO_ACCESS_VPTR(lib.parser, parser);
  dyn_parse_bytes(stack.current, file, lib.args, parse);
  print_recursive(0, stack, stdout);
  free_stack(stack);
  putchar('\n');
}
