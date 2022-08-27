#pragma once
#include "vtable.h"
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
typedef struct {
	struct VTableHeader *vtable;
	union {
		void *data;
		char in_data[sizeof(void *)];
	};
} DynValue;

static inline void dyn_free(DynValue val) {
	if ((intptr_t)val.vtable & 1) {
		free(val.data);
	}
}

static inline struct VTableHeader *dyn_vtable(DynValue val) {
	return (struct VTableHeader *)((intptr_t)val.vtable & ~1);
}

static inline void *dyn_data(DynValue *val) {
	if ((intptr_t)val->vtable & 1) {
		return val->data;
	} else {
		return val->in_data;
	}
}

static inline DynValue dyn_parse_bytes(char *bytes, struct ParserArgImpl parser) {
	DynValue ret;
	int64_t status = parser.val_impl(NULL, &bytes, 0x3, &ret);
	if (status != 0) {
		ret.vtable = 0;
		ret.in_data[0] = (char)status;
	}
	return ret;
}

static inline DynValue dyn_deref(DynValue val) {
	DynValue ret;
	struct NominalVTable *vtable = (struct NominalVTable *)dyn_vtable(val);
	int64_t status = vtable->deref_impl(dyn_data(&val), 0x3, &ret);
	if (status != 0) {
		ret.vtable = 0;
		ret.in_data[0] = (char)status;
	}
	return ret;
}

static inline DynValue dyn_access_field(DynValue block, char *name) {
	struct BlockVTable *vtable = (struct BlockVTable *)dyn_vtable(block);
	char **start = vtable->fields->fields;
	int64_t (*access_impl)(void *, int64_t, void *) = NULL;
	for(size_t i = 0; i < vtable->fields->number_fields; i++) {
		char *current_name = start[i];
		if (!strcmp(current_name, name)) {
			access_impl = vtable->access_impl[i];
			break;
		}
	}
	if (!access_impl) {
		return (DynValue){0};
	}
	DynValue ret;
	int64_t status = access_impl(dyn_data(&block), 0x3, &ret);
	if (status != 0) {
		ret.vtable = 0;
		ret.in_data[0] = (char)status;
	}
	return ret;
}

static inline int64_t dyn_int(DynValue integer) {
	return *(int64_t *)dyn_data(&integer);
}

static inline int32_t dyn_char(DynValue chr) {
	return *(int32_t *)dyn_data(&chr);
}

static inline int8_t dyn_bit(DynValue bit) {
	return *(int8_t *)dyn_data(&bit);
}
