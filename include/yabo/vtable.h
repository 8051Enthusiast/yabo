#pragma once
#include<stdint.h>
#include<sys/types.h>
enum YaboHead {
	YABO_INTEGER = 0x100,
	YABO_BIT = 0x200,
	YABO_CHAR = 0x300,
	YABO_LOOP = 0x400,
	YABO_PARSER = 0x500,
	YABO_FUN_ARGS = 0x600,
	YABO_BLOCK = 0x700,
};

enum ReturnStatus {
	OK = 0,
	ERROR = 1,
	EOS = 2,
	BACKTRACK = 3,
};

typedef struct {
	int64_t head;
	int64_t (*typecast_impl)(void *, int64_t, void *);
	size_t size;
	size_t align;
} VTableHeader;

typedef struct {
	char *name;
	size_t discriminant_offset;
} BlockFieldDescription;

typedef struct {
	size_t number_fields;
	BlockFieldDescription fields[];
} BlockFields ;

typedef struct {
	VTableHeader head;
	BlockFields *fields;
	int64_t (*access_impl[])(void *, int64_t, void *);
} BlockVTable;


typedef struct {
	VTableHeader head;
	int64_t (*deref_impl)(void *, int64_t, void *);
	int64_t (*start_impl)(void *, void *);
	int64_t (*end_impl)(void *, void *);
} NominalVTable;


typedef struct {
	int64_t (*val_impl)(void *, void *, int64_t, void *);
	int64_t (*len_impl)(void *, void *, void *);
} ParserArgImpl;


typedef struct {
	VTableHeader head;
	ParserArgImpl apply_table[];
} ParserVTable;


typedef struct {
	VTableHeader head;
	int64_t (*single_forward_impl)(void *, void *);
	int64_t (*current_element_impl)(void *, int64_t, void *);
	int64_t (*skip_impl)(void *, uint64_t, void *);
} ArrayVTable;
