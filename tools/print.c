#include <unistd.h>
#include <inttypes.h>
#include <stdio.h>
#include <sys/mman.h>
#include <yabo/vtable.h>
#include <yabo/dynamic.h>
#include <dlfcn.h>

#define fputc_ret(chr, file) {if (fputc(chr, file) == EOF) return EOF;}
#define rec_ret(fun, val, indent, out) {if (fun(val, indent, out) == EOF) return EOF;}

static inline int print_indent(int indent, FILE *out) {
	for (int i = 0; i < indent; i++) {
		fputc_ret(' ', out);
	}
	return 0;
}

int print_recursive(DynValue val, int indent, FILE *out);

int print_char(DynValue val, int indent, FILE *out) {
	int32_t char_value = dyn_char(val);
	fputc_ret('"', out);
	if (char_value < 0x80) {
		fputc_ret(char_value, out);
	} else if (char_value < 0x800) {
		fputc_ret(0xc0 | char_value >> 6, out);
		fputc_ret(0x80 | 0x3f & char_value, out);
	} else if (char_value < 0x10000) {
		fputc_ret(0xe0 | char_value >> 12, out);
		fputc_ret(0x80 | 0x3f & char_value >> 6, out);
		fputc_ret(0x80 | 0x3f & char_value, out);
	} else {
		fputc_ret(0xf0 | char_value >> 18, out);
		fputc_ret(0x80 | 0x3f & char_value >> 12, out);
		fputc_ret(0x80 | 0x3f & char_value >> 6, out);
		fputc_ret(0x80 | 0x3f & char_value, out);
	}
	fputc_ret('"', out);
	return 0;
}

int print_int(DynValue val, int indent, FILE *out) {
	int64_t int_value = dyn_int(val);
	return fprintf(out, "%" PRId64, int_value);
}

int print_bit(DynValue val, int indent, FILE *out) {
	int8_t bit = dyn_bit(val);
	char *text;
	if (bit) {
		text = "true";
	} else {
		text = "false";
	}
	return fputs(text, out);
}

int print_loop(DynValue val, int indent, FILE *out) {
	// not implemented
	return fputs("[]", out);
}

int print_parser(DynValue val, int indent, FILE *out) {
	// not really much we can print
	return fputs("\"parser\"", out);
}

int print_fun_args(DynValue val, int indent, FILE *out) {
	// not really much we can print
	return fputs("\"fun_args\"", out);
}

int print_block(DynValue val, int indent, FILE *out) {
	BlockVTable *vtable = (BlockVTable *)dyn_vtable(val);
	BlockFieldDescription *field_desc = vtable->fields->fields;
	BlockFieldDescription *field_end = field_desc + vtable->fields->number_fields;
	int status;
	if (fputs("{\n", out) == EOF) return EOF;
	int64_t (**access_impl)(void *, int64_t, void *) = vtable->access_impl;
	while (field_desc != field_end) {
		if (print_indent(indent + 2, out) == EOF) return EOF;
		if (fprintf(out, "\"%s\": ", field_desc->name) < 0) return EOF;
		DynValue sub_value;
		int64_t return_val = (*access_impl)(dyn_data(&val), 0x3, &sub_value);
		if (return_val != 0) {
			return -1;
		}
		if (print_recursive(sub_value, indent + 2, out) < 0) return EOF;
		if (fputs(",\n", out) == EOF) return EOF;
		access_impl++;
		field_desc++;
	}
	if (print_indent(indent, out) == EOF) return EOF;
	fputc_ret('}', out);
	return 0;
}

int print_nominal(DynValue val, int indent, FILE *out) {
	DynValue deref = dyn_deref(val);
	return print_recursive(deref, indent, out);
}

// frees val
int print_recursive(DynValue val, int indent, FILE *out) {
	int status;
	if (!val.vtable) {
		return fputs("null", out);
	}
	VTableHeader *vtable = dyn_vtable(val);
	int64_t head = vtable->head;
	if (head < 0) {
		status = print_nominal(val, indent, out);
	} else {
		switch (head) {
		case YABO_INTEGER:
			status = print_int(val, indent, out); break;
		case YABO_BIT:
			status = print_bit(val, indent, out); break;
		case YABO_CHAR:
			status = print_char(val, indent, out); break;
		case YABO_LOOP:
			status = print_loop(val, indent, out); break;
		case YABO_PARSER:
			status = print_parser(val, indent, out); break;
		case YABO_FUN_ARGS:
			status = print_fun_args(val, indent, out); break;
		case YABO_BLOCK:
			status = print_block(val, indent, out); break;
		}
	}
	dyn_free(val);
	return status;
}

void *map_file(char *filename) {
	FILE *f = fopen(filename, "r");
	if (!f) {
		perror("could not open file");
		return NULL;
	}
	if (fseek(f, 0, SEEK_END)) {
		perror("could not get file size");
		return NULL;
	}
	long length = ftell(f);
	if (length < 0) {
		perror("could not get file size");
		return NULL;
	}
	int fd = fileno(f);
	if (fd < 0) {
		perror("could not mmap file");
		return NULL;
	}
	void *ret = mmap(NULL, (size_t)length, PROT_READ, MAP_SHARED, fd, 0);
	if (ret == MAP_FAILED) {
		perror("could not mmap file");
		return NULL;
	}
	return ret;
}

int main(int argc, char **argv) {
	if (argc != 4) {
		fprintf(stderr, "usage: %s SOFILE PARSERNAME FILE\n", argv[0]);
		exit(1);
	}
	void *lib = dlopen(argv[1], RTLD_NOW);
	if (!lib) {
		perror("could not open library");
		exit(1);
	}
	ParserArgImpl *parser = (ParserArgImpl *)dlsym(lib, argv[2]);
	if (!parser) {
		perror("could not find parser");
		exit(1);
	}
	char *file = (char *)map_file(argv[3]);
	if (!file) {
		exit(1);
	}
	DynValue val = dyn_parse_bytes(file, *parser);
	print_recursive(val, 0, stdout);
	putchar('\n');
}
