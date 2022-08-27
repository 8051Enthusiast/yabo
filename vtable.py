import ctypes
import os
import sys
from ctypes import addressof, c_int64, c_ubyte, c_uint64, c_size_t, c_char, Structure, CFUNCTYPE, POINTER, byref, c_void_p, pointer

YABO_INTEGER = 256
YABO_BIT = 512
YABO_CHAR = 768
YABO_LOOP = 1024
YABO_PARSER = 1280
YABO_FUN_ARGS = 1536
YABO_BLOCK = 1792

OK = 0
ERROR = 1
EOS = 2
BACKTRACK = 3

_voidptr = ctypes.c_void_p


class BacktrackError(Exception):
    pass


class VTableHeader(Structure):
    __slots__ = [
        'head',
        'typecast_impl',
        'size',
        'align',
    ]
    _fields_ = [
        ('head', c_int64),
        ('typecast_impl', CFUNCTYPE(c_int64, _voidptr, c_int64, _voidptr)),
        ('size', c_size_t),
        ('align', c_size_t),
    ]


class BlockFields(Structure):
    __slots__ = [
        'number_fields',
        'fields',
    ]
    _fields_ = [
        ('number_fields', c_size_t),
        ('fields', POINTER(c_char) * 0),
    ]
    def __len__(self):
        return self.number_fields.value
    def __getitem__(self, index):
        if index > len(self):
            raise IndexError(index)
        offset = ctypes.sizeof(self) + index * ctypes.sizeof(c_void_p)
        return POINTER(c_char).from_buffer(self, offset)


class BlockVTable(Structure):
    __slots__ = [
        'head',
        'fields',
        'access_impl',
    ]
    _fields_ = [
        ('head', VTableHeader),
        ('fields', POINTER(BlockFields)),
        ('access_impl', POINTER(CFUNCTYPE(c_int64, _voidptr, c_int64, _voidptr)) * 0),
    ]

    def __len__(self):
        return len(self.fields.contents)

    def __getitem__(self, index):
        if index > len(self):
            raise IndexError(index)
        offset = ctypes.sizeof(self) + index * ctypes.sizeof(c_void_p)
        return POINTER(CFUNCTYPE(c_int64, _voidptr, c_int64, _voidptr)).from_buffer(self, offset)


class NominalVTable(Structure):
    __slots__ = [
        'head',
        'deref_impl',
        'start_impl',
        'end_impl',
    ]
    _fields_ = [
        ('head', VTableHeader),
        ('deref_impl', CFUNCTYPE(c_int64, _voidptr, c_int64, _voidptr)),
        ('start_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr)),
        ('end_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr)),
    ]


class ParserArgImpl(Structure):
    __slots__ = [
        'val_impl',
        'len_impl',
    ]
    _fields_ = [
        ('val_impl', CFUNCTYPE(c_int64, _voidptr,
         _voidptr, c_int64, _voidptr)),
        ('len_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr, _voidptr)),
    ]


class ParserVTable(Structure):
    __slots__ = [
        'head',
        'apply_table',
    ]
    _fields_ = [
        ('head', VTableHeader),
        ('apply_table', POINTER(ParserArgImpl)),
    ]


class ArrayVTable(Structure):
    __slots__ = [
        'head',
        'single_forward_impl',
        'current_element_impl',
        'skip_impl',
    ]
    _fields_ = [
        ('head', VTableHeader),
        ('single_forward_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr)),
        ('current_element_impl', CFUNCTYPE(
            c_int64, _voidptr, c_int64, _voidptr)),
        ('skip_impl', CFUNCTYPE(c_int64, _voidptr, c_uint64, _voidptr)),
    ]


class DynValue(Structure):
    __slots__ = [
        'vtable',
        'data',
    ]
    _fields_ = [
        ('vtable', POINTER(VTableHeader)),
        ('data', POINTER(c_ubyte)),
    ]

    def vtable_has_tag(self) -> bool:
        return addressof(self.vtable) & 1 != 0

    def get_vtable(self):
        ptr_int_val = addressof(self.vtable) & ~1
        return POINTER(VTableHeader).from_address(ptr_int_val).contents

    def data_ptr(self):
        if self.vtable_has_tag():
            return self.data
        offset = DynValue.data.offset
        array_ptr = pointer(
            (c_char * DynValue.data.size).from_buffer(self, offset))
        return ctypes.cast(array_ptr, _voidptr)


def _check_status(status: int):
    if status == OK:
        return
    elif status == EOS:
        raise EOFError
    elif status == BACKTRACK:
        raise BacktrackError
    else:
        raise Exception


class YaboValue:
    _val: DynValue
    _buf: bytearray

    def __init__(self, val: DynValue, buf: bytearray):
        self._val = val
        self._buf = buf

    def _typecast(self, typ: int):
        typecast = self._val.get_vtable().typecast_impl
        ret = DynValue()
        null_ptr = ctypes.c_void_p(0)
        status = typecast(self._val.data_ptr(), typ, byref(ret))
        _check_status(status)
        return YaboValue(ret, self._buf)


class Parser:
    impl: ParserArgImpl

    def __init__(self, impl: ParserArgImpl):
        self.impl = impl

    def parse(self, buf: bytearray) -> YaboValue:
        parse = self.impl.val_impl
        buffer_ptr = pointer((c_ubyte * len(buf)).from_buffer(buf))
        ret = DynValue()
        nullptr = ctypes.c_void_p()
        status = parse(nullptr, byref(buffer_ptr), 3, byref(ret))
        _check_status(status)
        return YaboValue(ret, buf)


class NominalValue(YaboValue):
    def __init__(self, val: DynValue, buf: bytearray):
        super().__init__(val, buf)

    def deref(self):
        casted_vtable = ctypes.cast(
            pointer(self._val.get_vtable()), POINTER(NominalVTable))
        deref = casted_vtable.contents.deref_impl
        ret = DynValue()
        status = deref(self._val.data_ptr(), 3, byref(ret))
        _check_status(status)
        return YaboValue(ret, self._buf)


class BlockValue(YaboValue):
    def __ini__(self, val: DynValue, buf: bytearray):
        super().__init__(val, buf)
        casted_vtable = ctypes.cast(
            pointer(self._val.get_vtable()), POINTER(BlockVTable))
#        for i in range(casted_vtable.contents.fields.contents.number_fields.value):
#            field = casted_vtable.contents.fields.contents.fields[i]
#            setattr(self, field.name, field.type)


class YaboLib(ctypes.CDLL):
    def __init__(self, name: str):
        super().__init__(name)

    def parser(self, name: str) -> Parser:
        impl = ParserArgImpl.in_dll(self, name)
        return Parser(impl)
