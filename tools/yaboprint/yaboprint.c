#include <inttypes.h>
#include <stdio.h>

#if LLUBI_COMPATIBLE
#if !defined(STATIC_FILE) || !defined(STATIC_PARSER)
#error "LLUBI_COMPATIBLE requires STATIC_FILE and STATIC_PARSER to be set"
#endif
#define eprintf(...) printf(__VA_ARGS__)
#define YABO_RELATIVE_VPTR 0
#else
#include <dlfcn.h>
#include <sys/mman.h>

#define eprintf(...) fprintf(stderr, __VA_ARGS__)
#endif
#include <yabo/dynamic.h>
#include <yabo/parse_export_call.h>
#include <yabo/vtable.h>

typedef struct {
  struct Globals *globals;
  DynValue *current;
  char *limit;
} Stack;

// 16 MB stack
#define STACK_SIZE 1024 * 1024 * 16


Stack init_stack(size_t max_dyn_size, size_t globals_size) {
  Stack stack;
  if (max_dyn_size > STACK_SIZE) {
    eprintf("Max dyn size too large\n");
    exit(1);
  }
  stack.current = malloc(STACK_SIZE);
  if (!stack.current) {
    eprintf("Could not allocate stack\n");
    exit(1);
  }
  stack.limit = (char *)stack.current + STACK_SIZE;
  stack.limit -= max_dyn_size;
  stack.globals = malloc(globals_size);
  return stack;
}

void free_stack(Stack stack) {
  free(stack.current);
  free(stack.globals);
}

Stack bump(Stack stack) {
  size_t size = dyn_val_size(stack.current);
  size_t aligned_size =
      (size + alignof(DynValue) - 1) & ~(alignof(DynValue) - 1);
  if ((size_t)(stack.limit - (char *)stack.current) < aligned_size) {
    eprintf("Value stack overflow\n");
    exit(1);
  }
  stack.current = (DynValue *)((char *)stack.current + aligned_size);
  return stack;
}

#define fputc_ret(chr)                                                   \
  {                                                                            \
    if (printf("%c", chr) == EOF)                                               \
      return EOF;                                                              \
  }

static inline int print_indent(int indent) {
  for (int i = 0; i < indent; i++) {
    fputc_ret(' ');
  }
  return 0;
}

int print_recursive(int indent, Stack stack);

int print_char(DynValue *val, int indent, Stack stack) {
  int32_t char_value = dyn_char(val);
  fputc_ret('"');
  if (char_value < 0x80) {
    fputc_ret(char_value);
  } else if (char_value < 0x800) {
    fputc_ret(0xc0 | char_value >> 6);
    fputc_ret(0x80 | (0x3f & char_value));
  } else if (char_value < 0x10000) {
    fputc_ret(0xe0 | char_value >> 12);
    fputc_ret(0x80 | (0x3f & (char_value >> 6)));
    fputc_ret(0x80 | (0x3f & char_value));
  } else {
    fputc_ret(0xf0 | char_value >> 18);
    fputc_ret(0x80 | (0x3f & (char_value >> 12)));
    fputc_ret(0x80 | (0x3f & (char_value >> 6)));
    fputc_ret(0x80 | (0x3f & char_value));
  }
  fputc_ret('"');
  return 0;
}

int print_int(DynValue *val, int indent, Stack stack) {
  int64_t int_value = dyn_int(val);
  return printf("%" PRId64, int_value);
}

int print_bit(DynValue *val, int indent, Stack stack) {
  int8_t bit = dyn_bit(val);
  const char *text;
  if (bit) {
    text = "true";
  } else {
    text = "false";
  }
  return printf("%s", text);
}

int print_parser(DynValue *val, int indent, Stack stack) {
  struct ParserVTable *vtable = (struct ParserVTable *)val->vtable;
  int64_t len;
  int64_t ret = YABO_ACCESS_VPTR(vtable, len_impl)(&len, val->data,
                                                   (const char *)stack.globals);
  if (ret != YABO_STATUS_OK) {
    return printf("\"parser\"");
  } else {
    return printf("\"parser(%" PRId64 ")\"", len);
  }
}

int print_fun_args(DynValue *val, int indent, Stack stack) {
  // not really much we can print
  return printf("\"fun_args\"");
}

int print_block(DynValue *val, int indent, Stack stack) {
  size_t count = dyn_block_field_count(val);
  if (puts("{") == EOF)
    return EOF;

  int first = 1;
  for (size_t i = 0; i < count; i++) {
    DynValue *sub_value = (DynValue *)stack.current;
    int64_t return_val =
        dyn_access_field_index(sub_value, val, i, stack.globals);
    if (return_val == 3) {
      continue;
    }
    if (return_val != 0) {
      return -1;
    }

    const char *field_desc = dyn_block_field_name_at_index(val, i);

    if (!first) {
      if (puts(",") == EOF) {
        return EOF;
      }
    }

    if (print_indent(indent + 2) == EOF)
      return EOF;
    if (printf("\"%s\": ", field_desc) < 0)
      return EOF;
    if (print_recursive(indent + 2, stack) < 0)
      return EOF;

    first = 0;
  }
  if (puts("") == EOF) {
    return EOF;
  }
  if (print_indent(indent) == EOF)
    return EOF;
  fputc_ret('}');
  return 0;
}

int print_array(DynValue *val, int indent, Stack stack) {
  int64_t len = dyn_array_len(val, stack.globals);
  if (puts("[") == EOF)
    return EOF;
  for (int64_t i = 0; i < len; i++) {
    DynValue *sub_value = stack.current;
    dyn_array_current_element(sub_value, val, stack.globals);
    if (i) {
      if (puts(",") == EOF) {
        return EOF;
      }
    }
    if (print_indent(indent + 2) == EOF)
      return EOF;
    if (print_recursive(indent + 2, stack) < 0)
      return EOF;
    dyn_array_single_forward(val, stack.globals);
  }
  if (puts("") == EOF) {
    return EOF;
  }
  if (print_indent(indent) == EOF)
    return EOF;
  fputc_ret(']');
  return 0;
}

int print_indirect(DynValue *val, int indent, Stack stack) {
  DynValue *deref = stack.current;
  dyn_deref(deref, val, stack.globals);
  return print_recursive(indent, stack);
}

int print_error(DynValue *val, int indent, Stack stack) {
  switch (dyn_error(val)) {
  case YABO_STATUS_ERROR:
    return printf("\"ERROR\"");
  case YABO_STATUS_EOS:
    return printf("\"EOS\"");
  case YABO_STATUS_BACKTRACK:
    return printf("null");
  default:
    return 0;
  }
}

int print_recursive(int indent, Stack stack) {
  int status;
  DynValue *val = stack.current;
  if (!val->vtable) {
    return print_error(val, indent, stack);
  }
  struct VTableHeader *vtable = val->vtable;
  dyn_mask(val);
  int64_t head = vtable->head & YABO_DISC_MASK;
  Stack substack = bump(stack);
  if (head == YABO_THUNK || head == YABO_U8) {
    status = print_indirect(val, indent, substack);
  } else {
    switch (head) {
    case YABO_INTEGER:
      status = print_int(val, indent, substack);
      break;
    case YABO_BIT:
      status = print_bit(val, indent, substack);
      break;
    case YABO_CHAR:
      status = print_char(val, indent, substack);
      break;
    case YABO_LOOP:
      status = print_array(val, indent, substack);
      break;
    case YABO_PARSER:
      status = print_parser(val, indent, substack);
      break;
    case YABO_FUN_ARGS:
      status = print_fun_args(val, indent, substack);
      break;
    case YABO_BLOCK:
      status = print_block(val, indent, substack);
      break;
    case YABO_UNIT:
      status = printf("\"unit\"");
      break;
    }
  }
  return status;
}

#if defined(STATIC_FILE)

const uint8_t static_file_content[] = {
#embed STATIC_FILE
};
// filename is ignored for static files
struct Slice map_file(char *_) {
  return (struct Slice){static_file_content,
                        static_file_content + sizeof(static_file_content)};
}

#else
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

#endif

struct LibInfo {
  size_t max_dyn_size;
  size_t global_size;
  InitFun *global_init;
  const struct ParserExport *parser;
  const void *args;
};

#if defined(STATIC_PARSER)

struct LibInfo static_lib() {
  struct LibInfo ret;
  extern size_t yabo_max_buf_size;
  extern size_t yabo_global_size;
  __attribute__((weak)) extern struct Slice yabo_global_address;
  extern InitFun yabo_global_init;
  extern struct ParserExport STATIC_PARSER;
  ret.max_dyn_size = yabo_max_buf_size;
  ret.global_size = yabo_global_size;
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
    eprintf("could not find phdr for phdr\n");
    exit(1);
  }

  if (!dynamic_phdr) {
    eprintf("could not find the dynamic segment\n");
    exit(1);
  }

  ptrdiff_t offset = phdr - phdr_phdr->p_vaddr;

  struct DynInfo info;
  if (!find_dyn_info(&info, dynamic_phdr, offset)) {
    eprintf("could not find dynamic info\n");
    exit(1);
  }

  struct LibInfo lib;
  lib.global_init = lookup_gnu_hash(&info, "yabo_global_init", offset);
  lib.parser = lookup_gnu_hash(&info, "main", offset);
  size_t *yabo_max_buf_size =
      lookup_gnu_hash(&info, "yabo_max_buf_size", offset);
  size_t *yabo_global_size =
      lookup_gnu_hash(&info, "yabo_global_size", offset);
  if (!lib.parser) {
    eprintf("could not find main function\n");
    exit(1);
  }
  if (!yabo_max_buf_size) {
    eprintf("could not find yabo_max_buf_size (is this a yabo library?)\n");
    exit(1);
  }
  if (!yabo_global_size) {
    eprintf("could not find yabo_global_size\n");
    exit(1);
  }
  lib.max_dyn_size = *yabo_max_buf_size;
  lib.global_size = *yabo_global_size;
  lib.args = NULL;
  return lib;
}

#else

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
    eprintf("unsupported export argument\n");
    exit(1);
  }

  void *args = malloc(size);
  if (!args) {
    eprintf("Could not allocate args");
    exit(1);
  }

  void *parser_args = parser_desc + yabo_export_identifier_end(parser_desc);
  enum YaboArgParseError err =
      yabo_export_parse_arg(parser_args, export_info, args);
  if (err) {
    eprintf("Could not parse args: %s\n",
            yabo_export_parse_error_message(err));
    exit(1);
  }
  return args;
}

struct LibInfo dynamic_lib(char *filename, char *parser_name) {
  struct LibInfo ret;
  void *lib = dlopen(filename, RTLD_NOW);
  if (!lib) {
    eprintf("could not open library: %s", dlerror());
    exit(1);
  }
  size_t *max_dyn_size_ptr = (size_t *)dlsym(lib, "yabo_max_buf_size");
  if (!max_dyn_size_ptr) {
    perror("could not find yabo_max_buf_size (is this a yabo library?)");
    exit(1);
  }
  size_t *global_size = (size_t *)dlsym(lib, "yabo_global_size");
  if (!global_size) {
    perror("could not find yabo_global_size");
    exit(1);
  }
  ret.max_dyn_size = *max_dyn_size_ptr;
  ret.global_size = *global_size;
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

#if LLUBI_COMPATIBLE
  if (argc != 1) {
    eprintf("usage: %s\n", argv[0]);
    exit(1);
  }

  struct LibInfo lib = static_lib();
#elif defined(STATIC_PARSER) || defined(ELF_INTERP)
  if (argc != 2) {
    eprintf("usage: %s FILE\n", argv[0]);
    exit(1);
  }

#ifdef STATIC_PARSER
  struct LibInfo lib = static_lib();
#else
  struct LibInfo lib = exec_lib();
#endif

#else

  if (argc != 4) {
    eprintf("usage: %s SOFILE PARSERNAME FILE\n", argv[0]);
    exit(1);
  }
  struct LibInfo lib = dynamic_lib(argv[1], argv[2]);

  if (!lib.parser) {
    eprintf("could not find parser: %s\n", dlerror());
    exit(1);
  }

#endif

  struct Slice file = map_file(argv[argc - 1]);
  if (!file.start) {
    exit(1);
  }

  Stack stack = init_stack(lib.max_dyn_size, lib.global_size);
  if (lib.global_init) {
    int64_t status =
        lib.global_init(file.start, file.end, (char *)stack.globals);
    if (status != 0) {
      eprintf("failed to initialize yabo library with status %" PRId64 "\n",
              status);
      exit(1);
    }
  }

  ParseFun *parse = YABO_ACCESS_VPTR(lib.parser, parser);
  dyn_parse_bytes(stack.current, file, lib.args, parse, stack.globals);
  print_recursive(0, stack);
  free_stack(stack);
  puts("");
}
