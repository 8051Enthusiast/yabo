"""
Python bindings to the dynamic interface of the yabo language.
"""

from copy import copy
from sys import byteorder
import ctypes
import threading
from os import getenv
from ctypes import (addressof, alignment, sizeof, c_char_p, c_int64,
                    c_int8, c_ubyte, c_uint32, c_uint64, c_size_t, c_char,
                    Structure, CFUNCTYPE, POINTER, byref, c_void_p, pointer)
import tempfile
from shutil import copyfileobj


YABO_DISC_MASK = ((1 << 64) - 1) & ~0xff

YABO_INTEGER = 0x100
YABO_BIT = 0x200
YABO_CHAR = 0x300
YABO_LOOP = 0x400
YABO_SLICE_PTR = 0x401
YABO_PARSER = 0x500
YABO_FUN_ARGS = 0x600
YABO_BLOCK = 0x700
YABO_UNIT = 0x800
YABO_U8 = 0x900
YABO_ANY = YABO_DISC_MASK
YABO_VTABLE = 1

YABO_GLOBAL_ADDRESS_NAME = "yabo_global_address"
YABO_GLOBAL_INIT_NAME = "yabo_global_init"

OK = 0
ERROR = 1
EOS = 2
BACKTRACK = 3

_voidptr = ctypes.c_void_p

MASK_TEST_MODE: bool = False

class MaskError(Exception):
    def __init__(self, left: bytearray, right: bytearray):
        self.left = left
        self.right = right
    
    def __str__(self):
        return f"MaskError: {self.left.hex()} != {self.right.hex()}"
    
    def __repr__(self):
        return f"MaskError({self.left}, {self.right})"

class BacktrackError(Exception):
    def __str__(self):
        return "BacktrackError"
    
    def __repr__(self):
        return "BacktrackError()"    


class VTableHeader(Structure):
    __slots__ = [
        'head',
        'deref_level',
        'typecast_impl',
        'mask_impl',
        'size',
        'align',
    ]
    _fields_ = [
        ('head', c_int64),
        ('deref_level', c_int64),
        ('typecast_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr, c_int64)),
        ('mask_impl', CFUNCTYPE(c_size_t, _voidptr)),
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
        'name',
        'start_impl',
        'end_impl',
    ]
    _fields_ = [
        ('head', VTableHeader),
        ('name', c_char_p),
        ('start_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr, c_int64)),
        ('end_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr, c_int64)),
    ]


PARSER_TY = CFUNCTYPE(c_int64, _voidptr, _voidptr, c_int64, _voidptr)
LEN_FUN_TY = CFUNCTYPE(c_int64, POINTER(c_int64), POINTER(c_ubyte))
INIT_TY = CFUNCTYPE(c_int64)

class ParserVTable(Structure):
    __slots__ = [
        'head',
        'len_impl',
        'apply_table',
    ]
    _fields_ = [
        ('head', VTableHeader),
        ('len_impl', LEN_FUN_TY),
        ('apply_table', POINTER(PARSER_TY)),
    ]


class ArrayVTable(Structure):
    __slots__ = [
        'head',
        'single_forward_impl',
        'current_element_impl',
        'array_len_impl',
        'skip_impl',
        'span_impl',
        'inner_array_impl',
    ]
    _fields_ = [
        ('head', VTableHeader),
        ('single_forward_impl', CFUNCTYPE(c_int64, _voidptr)),
        ('current_element_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr, c_int64)),
        ('array_len_impl', CFUNCTYPE(c_int64, _voidptr)),
        ('skip_impl', CFUNCTYPE(c_int64, _voidptr, c_uint64)),
        ('span_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr, c_uint64, _voidptr)),
        ('inner_array_impl', CFUNCTYPE(c_int64, _voidptr, _voidptr, c_int64)),
    ]


class ParserExport(Structure):
    __slots__ = [
        'parser',
        'args',
    ]
    _fields_ = [
        ('parser', PARSER_TY),
        ('args', POINTER(POINTER(VTableHeader)) * 0),
    ]

    def get_arg_count(self) -> int:
        count = 0
        offset = ctypes.sizeof(self)
        while True:
            addr = addressof(self) + offset + count * ctypes.sizeof(POINTER(VTableHeader))
            arg_ptr = POINTER(VTableHeader).from_address(addr)
            if not arg_ptr:
                break
            count += 1
        return count

    def get_arg_types(self) -> list[int]:
        arg_types = []
        count = self.get_arg_count()
        offset = ctypes.sizeof(self)
        for i in range(count):
            addr = addressof(self) + offset + i * ctypes.sizeof(POINTER(VTableHeader))
            head = POINTER(VTableHeader).from_address(addr).contents.head
            arg_types.append(head)
        return arg_types

MAX_ALIGNMENT = max(alignment(_voidptr), alignment(c_int64))

class Slice(Structure):
    __slots__ = [
        'start',
        'end',
    ]
    _fields_ = [
        ('start', POINTER(c_ubyte)),
        ('end', POINTER(c_ubyte)),
    ]

    def __init__(self, buf: bytearray):
        start = (c_ubyte * len(buf)).from_buffer(buf)
        self.start = ctypes.cast(pointer(start), POINTER(c_ubyte))
        end = (c_ubyte * 0).from_buffer(buf, len(buf))
        self.end = ctypes.cast(pointer(end), POINTER(c_ubyte))

class DynValue(Structure):
    def get_vtable(self):
        raise NotImplementedError
    
    def data_array(self):
        raise NotImplementedError

    def data_ptr(self):
        array_ptr = self.data_array()
        return ctypes.cast(array_ptr, _voidptr)

    def mask(self):
        mask_impl = self.get_vtable().mask_impl
        return mask_impl(self.data_ptr())

def sized_dyn_value(size: int):
    class SizedDynValue(DynValue):
        __slots__ = [
            'padding',
            'vtable',
            'data',
        ]
        _fields_ = [
            ('padding', c_char * (MAX_ALIGNMENT - sizeof(_voidptr))),
            ('vtable', POINTER(VTableHeader)),
            ('data', c_char * size),
        ]
        def __copy__(self):
            size = self.get_vtable().size
            val_ty = sized_dyn_value(size)
            val = val_ty.from_buffer_copy(self)
            return val

        def __deepcopy__(self, memo):
            return self.__copy__()

        def get_vtable(self):
            return self.vtable.contents

        def data_array(self):
            offset = type(self).data.offset
            return pointer((c_char * type(self).data.size).from_buffer(self, offset))

    return SizedDynValue


def _check_status(status: int):
    if status == OK:
        return
    if status == EOS:
        raise EOFError
    if status == BACKTRACK:
        raise BacktrackError
    raise Exception


def _pack_export_args(args: list, export_info: ParserExport) -> bytearray:
    arg_types = export_info.get_arg_types()

    if len(args) != len(arg_types):
        if len(args) < len(arg_types):
            raise ValueError("not enough arguments provided")
        else:
            raise ValueError("too many arguments provided")

    result = bytearray()
    for i, (arg_value, arg_type) in enumerate(zip(args, arg_types)):
        if arg_type != YABO_INTEGER:
            raise ValueError("unsupported type - only integer arguments supported")

        if not isinstance(arg_value, int):
            raise ValueError(f"argument {i+1} must be an integer")

        if arg_value < -(2**63) or arg_value >= 2**63:
            raise ValueError(f"argument {i+1} out of range for int64")

        result.extend(arg_value.to_bytes(8, byteorder=byteorder, signed=True))

    return result


class Parser:
    export_info: ParserExport
    _lib: "YaboLib"

    def __init__(self, export_info: ParserExport, lib: "YaboLib"):
        self.export_info = export_info
        self._lib = lib

    def parse(self, buf: bytearray, *args):
        parse = self.export_info.parser

        if args:
            try:
                arg_data = _pack_export_args(list(args), self.export_info)
                args_ptr = (ctypes.c_char * len(arg_data)).from_buffer(arg_data)
                args_void_ptr = ctypes.cast(args_ptr, ctypes.c_void_p)
            except ValueError as e:
                raise ValueError(f"Argument error: {e}")
        else:
            if self.export_info.get_arg_count() > 0:
                raise ValueError("Parser expects arguments but none provided")
            args_void_ptr = ctypes.c_void_p()

        return self._lib.new_val(lambda ret: parse(ret, args_void_ptr, self._lib.discriminant(), byref(Slice(buf))))


class YaboLib(ctypes.CDLL):
    _loc: threading.local
    _autoderef: bool
    def __init__(self, path: str, global_address: bytearray, autoderef: bool=True):
        self._autoderef = autoderef
        # we need to copy the library to a temporary path because
        # loading the same library twice will deduplicates globals
        # and cause a mess if it is overwritten
        with tempfile.NamedTemporaryFile() as tmp_file:
            with open(path, 'rb') as f:
                copyfileobj(f, tmp_file)
            super().__init__(tmp_file.name)
        slice_addr = Slice(global_address)
        status = self[YABO_GLOBAL_INIT_NAME](slice_addr.start, slice_addr.end)
        _check_status(status)
        
        self._loc = threading.local()

    def discriminant(self) -> int:
        if self._autoderef:
            return YABO_VTABLE
        else:
            return YABO_ANY | YABO_VTABLE

    def parser(self, name: str) -> Parser:
        export_info = ParserExport.in_dll(self, name)
        return Parser(export_info, self)

    def _ret_buf(self):
        try:
            return self._loc.ret_buf
        except AttributeError:
            size = c_size_t.in_dll(self, "yabo_max_buf_size").value
            self._loc.ret_buf = sized_dyn_value(size)()
            return self._loc.ret_buf
    
    def new_val(self, f):
        ret_buf = self._ret_buf()
        status = f(ret_buf.data_ptr())
        _check_status(status)
        ret_val = _new_value(ret_buf, self)
        if MASK_TEST_MODE:
            # invert all bytes in the buffer so that we can
            # test that the mask implementation properly
            # deletes the padding bytes
            for i in range(ret_buf.get_vtable().size):
                ret_buf.data_array().contents[i] = ret_buf.data_array().contents[:][i] ^ 0xFF
            status = f(ret_buf.data_ptr())
            _check_status(status)
            second_val = _new_value(ret_buf, self)
            if isinstance(ret_val, YaboValue):
                first_bytes = bytes(ret_val._val.data_array().contents[:ret_val._val.get_vtable().size])
                second_bytes = bytes(second_val._val.data_array().contents[:second_val._val.get_vtable().size])
                if first_bytes != second_bytes:
                    raise MaskError(first_bytes, second_bytes)
        return ret_val
            

class YaboValue:
    _val: DynValue
    _lib: YaboLib
    _loc: threading.local

    def __init__(self, val: DynValue, lib: YaboLib):
        self._val = val
        self._lib = lib

    def _typecast(self, typ: int):
        typecast = self._val.get_vtable().typecast_impl
        return self._lib.new_val(lambda ret: typecast(ret, self._val.data_ptr(), typ))

    def __copy__(self):
        return self._typecast(self._val.get_vtable().deref_level | YABO_VTABLE)

    def __eq__(self, other):
        if not isinstance(other, YaboValue):
            return False
        return self._val.data_array().contents[:] == other._val.data_array().contents[:]
    
    def __hash__(self):
        return hash(self._val.data_array().contents[:])


class NominalValue(YaboValue):
    def deref(self):
        level = self._val.get_vtable().deref_level
        level = max(level - 0x100, 0)
        return self._typecast(level | YABO_VTABLE)


class BlockValue(YaboValue):
    _access_impl: dict

    def __init__(self, val: DynValue, lib: YaboLib):
        super().__init__(val, lib)
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
            return super().__getattribute__(name)
        return ret

    def get(self, name: str):
        try:
            access = self._access_impl[name]
        except KeyError:
            raise AttributeError(f'{name} is not a valid field')
        try:
            return self._lib.new_val(lambda ret:
                access(ret, self._val.data_ptr(), self._lib.discriminant())
            )
        except BacktrackError:
            return None

    def __getitem__(self, field: str):
        res = self.__getattr__(field)
        if res is None:
            raise AttributeError(f"{field} is not a field of {self}")
        return res

    def fields(self):
        return [field for field in self._access_impl if self.get(field) is not None]

    def __dir__(self):
        return sorted(dir(BlockValue) + list(self._access_impl.keys()))


class ArrayValue(YaboValue):
    def __len__(self):
        array_vtable = ctypes.cast(
            pointer(self._val.get_vtable()), POINTER(ArrayVTable))
        array_len_impl = array_vtable.contents.array_len_impl
        return array_len_impl(self._val.data_ptr())

    def skip(self, n: int):
        array_vtable = ctypes.cast(
            pointer(self._val.get_vtable()), POINTER(ArrayVTable))
        skip_impl = array_vtable.contents.skip_impl
        status = skip_impl(self._val.data_ptr(), n)
        _check_status(status)

    def current_element(self):
        array_vtable = ctypes.cast(
            pointer(self._val.get_vtable()), POINTER(ArrayVTable))
        current_element_impl = array_vtable.contents.current_element_impl
        return self._lib.new_val(lambda ret:
            current_element_impl(ret, self._val.data_ptr(), self._lib.discriminant())
        )

    def __getitem__(self, index: int):
        if len(self) <= index:
            raise IndexError(f'{index} is out of bounds')
        cop = copy(self)
        cop.skip(index)
        return cop.current_element()
    
    def as_list(self):
        return [self[i] for i in range(len(self))]

    def inner_array(self):
        array_vtable = ctypes.cast(
            pointer(self._val.get_vtable()), POINTER(ArrayVTable))
        inner_array_impl = array_vtable.contents.inner_array_impl
        return self._lib.new_val(lambda ret:
            inner_array_impl(ret, self._val.data_ptr(), self._lib.discriminant())
        )
    


class FunArgValue(YaboValue):
    pass


class ParserValue(YaboValue):
    def len(self):
        casted_vtable = ctypes.cast(
            pointer(self._val.get_vtable()), POINTER(ParserVTable))
        casted_data = ctypes.cast(
            self._val.data_ptr(), POINTER(c_ubyte))
        len_int = c_int64(0)
        ret = casted_vtable.contents.len_impl(POINTER(c_int64)(len_int),
                                                casted_data)
        _check_status(ret)
        return len_int.value


class UnitValue(YaboValue):
    pass

class U8Value(YaboValue):
    def deref(self):
        return self._typecast(0 | YABO_VTABLE)


def _new_value(val: DynValue, lib: YaboLib):
    val.mask()
    head = val.get_vtable().head
    if head not in [YABO_INTEGER, YABO_BIT, YABO_CHAR]:
        val = copy(val)
    if head < 0:
        return NominalValue(val, lib)
    masked_head = head & YABO_DISC_MASK
    if masked_head == YABO_INTEGER:
        return ctypes.cast(val.data_ptr(), POINTER(c_int64)).contents.value
    if masked_head == YABO_BIT:
        return bool(ctypes.cast(val.data_ptr(), POINTER(c_int8)).contents.value)
    if masked_head == YABO_CHAR:
        return chr(ctypes.cast(val.data_ptr(), POINTER(c_uint32)).contents.value)
    if masked_head == YABO_LOOP:
        return ArrayValue(val, lib)
    if masked_head == YABO_PARSER:
        return ParserValue(val, lib)
    if masked_head == YABO_FUN_ARGS:
        return FunArgValue(val, lib)
    if masked_head == YABO_BLOCK:
        return BlockValue(val, lib)
    if masked_head == YABO_UNIT:
        return UnitValue(val, lib)
    if masked_head == YABO_U8:
        return U8Value(val, lib)
    raise Exception("Unknown type")
