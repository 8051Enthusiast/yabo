import ctypes
from ctypes import (addressof, c_char_p, c_int64, c_int8, c_ubyte,
                    c_uint32, c_uint64, c_size_t, c_char, Structure,
                    CFUNCTYPE, POINTER, byref, c_void_p, pointer)

YABO_INTEGER = 0x100
YABO_BIT = 0x200
YABO_CHAR = 0x300
YABO_LOOP = 0x400
YABO_PARSER = 0x500
YABO_FUN_ARGS = 0x600
YABO_BLOCK = 0x700
YABO_UNIT = 0x800
YABO_ANY = ((1 << 64) - 1) & ~0xff
YABO_MALLOC = 0x3

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
        'deref_level',
        'typecast_impl',
        'size',
        'align',
    ]
    _fields_ = [
        ('head', c_int64),
        ('deref_level', c_size_t),
        ('typecast_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr, c_int64)),
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
        ('fields', c_char_p * 0),
    ]

    def __len__(self) -> int:
        return self.number_fields

    def __getitem__(self, index):
        if index > len(self):
            raise IndexError(index)
        offset = ctypes.sizeof(self) + index * ctypes.sizeof(c_void_p)
        addr = addressof(self) + offset
        return c_char_p.from_address(addr).value.decode()


class BlockVTable(Structure):
    __slots__ = [
        'head',
        'fields',
        'access_impl',
    ]
    _fields_ = [
        ('head', VTableHeader),
        ('fields', POINTER(BlockFields)),
        ('access_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr, c_int64) * 0),
    ]

    def __len__(self) -> int:
        return len(self.fields.contents)

    def __getitem__(self, index):
        if index > len(self):
            raise IndexError(index)
        offset = ctypes.sizeof(self) + index * \
            ctypes.sizeof(self.access_impl._type_)
        addr = addressof(self) + offset
        return self.access_impl._type_.from_address(addr)


class NominalVTable(Structure):
    __slots__ = [
        'head',
        'deref_impl',
        'start_impl',
        'end_impl',
    ]
    _fields_ = [
        ('head', VTableHeader),
        ('deref_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr, c_int64)),
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
        return addressof(self.vtable.contents) & 1 != 0

    def get_vtable(self):
        ptr_int_val = addressof(self.vtable.contents) & ~1
        return VTableHeader.from_address(ptr_int_val)

    def data_field_ptr(self):
        offset = DynValue.data.offset
        array_ptr = pointer(
            (c_char * DynValue.data.size).from_buffer(self, offset))
        return ctypes.cast(array_ptr, _voidptr)

    def data_ptr(self):
        if self.vtable_has_tag():
            return self.data
        return self.data_field_ptr()


def _check_status(status: int):
    if status == OK:
        return
    if status == EOS:
        raise EOFError
    if status == BACKTRACK:
        raise BacktrackError
    raise Exception


class Parser:
    impl: ParserArgImpl

    def __init__(self, impl: ParserArgImpl, lib):
        self.impl = impl
        self._lib = lib

    def parse(self, buf: bytearray):
        parse = self.impl.val_impl
        buffer_ptr = pointer((c_ubyte * len(buf)).from_buffer(buf))
        ret = DynValue()
        nullptr = ctypes.c_void_p()
        # the vtable pointer is stored at negative index 1, so we pass
        # a pointer to the data field
        status = parse(nullptr, byref(buffer_ptr), YABO_ANY |
                       YABO_MALLOC, ret.data_field_ptr())
        _check_status(status)
        return _new_value(ret, buf, self._lib)


class YaboLib(ctypes.CDLL):
    def __init__(self, name: str):
        super().__init__(name)

    def parser(self, name: str) -> Parser:
        impl = ParserArgImpl.in_dll(self, name)
        return Parser(impl, self)


class YaboValue:
    _val: DynValue
    _buf: bytearray
    _lib: YaboLib

    def __init__(self, val: DynValue, buf: bytearray, lib: YaboLib):
        self._val = val
        self._buf = buf
        self._lib = lib

    def _typecast(self, typ: int):
        typecast = self._val.get_vtable().typecast_impl
        ret = DynValue()
        status = typecast(ret.data_field_ptr(), self._val.data_ptr(), typ)
        _check_status(status)
        return YaboValue(ret, self._buf, self._lib)

    # yes i'm defining the evil function
    def __del__(self):
        if self._val.vtable_has_tag():
            dataptr = self._val.data_ptr()
            # set the vtable to the null pointer to avoid double frees
            # (this clears the tag)
            self._val.vtable = ctypes.cast(
                ctypes.c_void_p(0), POINTER(VTableHeader))
            free = self._lib.yabo_free
            free(dataptr)


class NominalValue(YaboValue):
    def deref(self):
        casted_vtable = ctypes.cast(
            pointer(self._val.get_vtable()), POINTER(NominalVTable))
        deref = casted_vtable.contents.deref_impl
        ret = DynValue()
        status = deref(ret.data_field_ptr(), self._val.data_ptr(), YABO_ANY | YABO_MALLOC)
        _check_status(status)
        return _new_value(ret, self._buf, self._lib)


class BlockValue(YaboValue):
    _access_impl: dict

    def __init__(self, val: DynValue, buf: bytearray, lib: YaboLib):
        super().__init__(val, buf, lib)
        casted_vtable = ctypes.cast(
            pointer(self._val.get_vtable()), POINTER(BlockVTable))
        self._access_impl = {}
        for i in range(len(casted_vtable.contents)):
            field_name = casted_vtable.contents.fields.contents[i]
            field_access = casted_vtable.contents[i]
            self._access_impl[field_name] = field_access

    def __getattr__(self, name: str):
        ret = self.get(name)
        if ret is None:
            raise AttributeError(f"{name} is not a field of {self}")
        return ret

    def get(self, name: str):
        try:
            access = self._access_impl[name]
        except KeyError:
            raise AttributeError(f'{name} is not a valid field')
        ret = DynValue()
        status = access(ret.data_field_ptr(), self._val.data_ptr(),
                        YABO_ANY | YABO_MALLOC)
        if status == BACKTRACK:
            return None
        _check_status(status)
        return _new_value(ret, self._buf, self._lib)

    def __getitem__(self, field: str):
        return self.__getattr__(field)

    def fields(self):
        return [field for field in self._access_impl if self.get(field) is not None]


class ArrayValue(YaboValue):
    pass


class FunArgValue(YaboValue):
    pass


class ParserValue(YaboValue):
    pass


class UnitValue(YaboValue):
    pass


def _new_value(val: DynValue, buf: bytearray, lib: YaboLib):
    head = val.get_vtable().head
    if head == YABO_INTEGER:
        return ctypes.cast(val.data_ptr(), POINTER(c_int64)).contents.value
    if head == YABO_BIT:
        return bool(ctypes.cast(val.data_ptr(), POINTER(c_int8)).contents.value)
    if head == YABO_CHAR:
        return chr(ctypes.cast(val.data_ptr(), POINTER(c_uint32)).contents.value)
    if head == YABO_LOOP:
        return ArrayValue(val, buf, lib)
    if head == YABO_PARSER:
        return ParserValue(val, buf, lib)
    if head == YABO_FUN_ARGS:
        return FunArgValue(val, buf, lib)
    if head == YABO_BLOCK:
        return BlockValue(val, buf, lib)
    if head == YABO_UNIT:
        return UnitValue(val, buf, lib)
    if head < 0:
        return NominalValue(val, buf, lib)
    raise Exception("Unknown type")
