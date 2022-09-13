#!/usr/bin/env python3
from collections import defaultdict
import os
import sys
import shutil
import re
import tempfile
import subprocess
import json
from typing import Optional, Tuple
import yabo

TARGET_RELEASE = 'debug'
BINARY_NAME = 'yabo'

# test files are structured as follows:
# ====[ source code ]====
# (source)
# ====[ binary 1 ]====
# (hexdump of input)
# ====[ output 1 ]====
# (json of output)
#
# this regex will match the title lines to split it
heading = re.compile('^={3,}\[\s*(.*?)\s*\]\={3,}$', re.MULTILINE)

# this regex matches a input/output title
case_title = re.compile('^(binary|output)\s+(.+)$')

# a comment that contains the error message
# for example, '//~^ error[301] not found here'
# represents an error message that should be
# one line above the line with the comment
# with error code 301
error_comment = re.compile('^.*//(~\^*)\s*error\[(\d+)\]\s*(.*)$')


class ErrorLocation:
    contained_message: str
    line: int
    code: int

    def __init__(self, message: str, line: int, code: int):
        self.contained_message = message
        self.line = line
        self.code = code

    def __str__(self) -> str:
        return f'ErrorLocation({self.contained_message}, {self.line}, {self.code})'

    @staticmethod
    def from_match(match: re.Match, line: int) -> 'ErrorLocation':
        contained_message = match.group(3)
        line = line - (len(match.group(1)) - 1)
        code = int(match.group(2))
        return ErrorLocation(contained_message, line, code)

    @staticmethod
    def from_diagnostics(diagnostic: dict) -> dict[int, list['ErrorLocation']]:
        code: int = diagnostic['code']
        ret = defaultdict(list)
        for message in diagnostic['labels']:
            line: int = message['start_line']
            message: int = f'{diagnostic["message"]}, {message["message"]}'
            ret[line].append(ErrorLocation(message, line, code))
        return ret

    def is_contained_in(self, other: 'ErrorLocation') -> bool:
        return (
            self.contained_message in other.contained_message and
            self.code == other.code and
            self.line == other.line
        )


current_script_dir = os.path.dirname(os.path.realpath(__file__))


def build_compiler_binary():
    compiler_dir = os.path.join(current_script_dir, 'compiler')
    os.chdir(compiler_dir)
    cargo_metadata = subprocess.Popen(
        ['cargo', 'metadata', '--format-version=1'], stdout=subprocess.PIPE)
    cargo_metadata_output = json.loads(
        cargo_metadata.communicate()[0].decode('utf-8'))
    cargo_args = ['cargo', 'build']
    if TARGET_RELEASE == 'release':
        cargo_args.append('--release')
    cargo_build = subprocess.run(cargo_args)
    if cargo_build.returncode != 0:
        raise Exception('Failed to build compiler')
    return os.path.join(cargo_metadata_output['target_directory'], TARGET_RELEASE, BINARY_NAME)


def run_compiler_unit_tests():
    compiler_dir = os.path.join(current_script_dir, 'compiler')
    os.chdir(compiler_dir)
    cargo_args = ['cargo', 'test']
    if TARGET_RELEASE == 'release':
        cargo_args.append('--release')
    cargo_test = subprocess.run(cargo_args)
    return cargo_test.returncode == 0


compiler_path = build_compiler_binary()


class CompiledSource:
    dir: str
    compiled: str
    source: str
    stderr: str

    def __init__(self, source):
        dir = tempfile.mkdtemp()
        self.source = source
        self.dir = dir
        try:
            self.compiled = os.path.join(dir, 'target.so')
            sourcepath = os.path.join(dir, 'source.yb')
            with open(sourcepath, 'w') as sourcefile:
                sourcefile.write(source)
            compiler = subprocess.Popen(
                [compiler_path, "--output-json", sourcepath, self.compiled], stderr=subprocess.PIPE)
            self.stderr = compiler.communicate()[1].decode('utf-8')
        except Exception as e:
            shutil.rmtree(self.dir)
            raise e

    def check_errors(self):
        diagnostics = defaultdict(list)
        for error in self.stderr.splitlines():
            error_json = json.loads(error)
            diagnostics.update(ErrorLocation.from_diagnostics(error_json))
        for (linenum, line) in enumerate(self.source.splitlines()):
            linenum = linenum + 1
            match = error_comment.match(line)
            if not match:
                continue
            expected = ErrorLocation.from_match(match, linenum)

            if expected.line not in diagnostics or len(diagnostics[expected.line]) == 0:
                raise Exception(
                    f'Expected error {expected.code} on line {expected.line}, but no error was found'
                )
            line_diagnostics = diagnostics[expected.line]
            found = False
            for actual in line_diagnostics:
                if expected.is_contained_in(actual):
                    found = True
                    break
            if not found:
                if len(line_diagnostics) == 1:
                    diag = line_diagnostics[0]
                    if diag.code != expected.code:
                        raise Exception(
                            f'Expected error {expected.code} on line {expected.line}, ' \
                            f'but found error {diag.code} instead'
                        )
                    else:
                        raise Exception(
                            f'Expected error message "{expected.contained_message}" on ' \
                            f'line {expected.line}, but found "{diag.contained_message}"'
                        )
                raise Exception(
                    f'Expected error {expected.code} on line {expected.line}, ' \
                    f'but none of the errors matched'
                )

    def has_errors(self) -> bool:
        return len(self.stderr) > 0

    def __enter__(self) -> str:
        return self.compiled

    def __exit__(self, _exc_type, _exc_value, _traceback):
        shutil.rmtree(self.dir)


class InputOutputPair:
    input: bytes
    output: str

    def __init__(self, input, output):
        self.input = input
        self.output = output


def dictionarified_obj(obj):
    while type(obj) is yabo.NominalValue:
        obj = obj.deref()
    ty = type(obj)
    if ty is int or ty is str or ty is bool:
        return obj
    if ty is yabo.ArrayValue:
        return "array"
    if ty is yabo.ParserValue:
        return "parser"
    if ty is yabo.FunArgValue:
        return "fun_args"
    if ty is yabo.BlockValue:
        ret_dict = {}
        for field in obj.fields():
            ret_dict[field] = dictionarified_obj(obj.get(field))
        return ret_dict


def wrap_maybe_field(inner: str, indent: str, field: Optional[str] = None):
    if field is None:
        return inner + '\n'
    return f'{indent}"{field}": {inner},\n'


def dict_with_indent(d, indent: str, field=None) -> str:
    if type(d) is str:
        ret = f'"{d}"'
    elif type(d) is int:
        ret = str(d)
    elif type(d) is bool:
        if d:
            ret = 'true'
        else:
            ret = 'false'
    elif type(d) is not dict:
        ret = str(d)
    else:
        ret = '{\n'
        sorted_keys = sorted(d.keys())
        for key in sorted_keys:
            value = d[key]
            ret += dict_with_indent(value, indent + '  ', key)
        ret += f'{indent}}}'
    return wrap_maybe_field(ret, indent, field)


RED = '\033[31m'
GREEN = '\033[32m'
CLEAR = '\033[0m'


class MatchingHead:
    __slots__ = ['data']

    def __init__(self, data):
        self.data = data

    def diff(self, indent='', field=None) -> str:
        return dict_with_indent(self.data, indent, field)


class DiffHead:
    __slots__ = ['left', 'right']

    def __init__(self, left, right):
        self.left = left
        self.right = right

    def diff(self, indent='', field=None) -> str:
        ret = ''
        if self.left is not None:
            ret += RED
            ret += dict_with_indent(self.left, indent, field)
        if self.right is not None:
            ret += GREEN
            ret += dict_with_indent(self.right, indent, field)
        return ret + CLEAR


class DictHead:
    __slot__ = ['data']
    data: dict

    def __init__(self, data):
        self.data = data

    def diff(self, indent='', field=None) -> str:
        out = '{\n'
        sorted_keys = sorted(self.data.keys())
        for key in sorted_keys:
            value = self.data[key]
            out += value.diff(indent + '  ', key)
        out += f'{indent}}}'
        return wrap_maybe_field(out, indent, field)


def diff(left, right) -> Tuple[MatchingHead | DiffHead | DictHead, bool]:
    if type(left) is dict and type(right) is dict:
        is_different = False
        ret = {}
        both_fields = set(left.keys()) | set(right.keys())
        for field in both_fields:
            left_field = left.get(field)
            right_field = right.get(field)
            (ret[field], is_field_diff) = diff(left_field, right_field)
            is_different |= is_field_diff
        return (DictHead(ret), is_different)

    if left != right:
        return (DiffHead(left, right), True)

    return (MatchingHead(left), False)


class TestFile:
    source: str
    cases: dict[str, InputOutputPair]

    def __init__(self, source: str):
        self.cases = {}
        self.source = source
        self.parse(source)

    # parses a raw file into a TestFile object
    def parse(self, source: str):
        split = heading.split(source)
        binary_cases = {}
        output_cases = {}
        if split[1] != 'source code':
            raise Exception('first section must be source')
        self.source = split[2]
        for (title, subtext) in zip(split[3::2], split[4::2]):
            match = case_title.fullmatch(title)
            if not match:
                raise Exception('Invalid title: ' + title)
            kind = match.group(1)
            test_name = match.group(2)
            if kind == 'binary':
                binary_cases[test_name] = subtext
            elif kind == 'output':
                output_cases[test_name] = subtext

        for (test_name, hexdump) in binary_cases.items():
            binary = bytes.fromhex(hexdump)
            try:
                output = output_cases.pop(test_name)
            except KeyError:
                raise Exception('No output for test case: ' + test_name)
            self.cases[test_name] = InputOutputPair(binary, output)

        if output_cases:
            raise Exception('Extra output cases: ' +
                            ', '.join(output_cases.keys()))

    def run(self) -> int:
        failed_tests = 0
        compiled_source = CompiledSource(self.source)
        if compiled_source.has_errors():
            try:
                compiled_source.check_errors()
            except Exception as e:
                print(f'{RED}{e}{CLEAR}')
                return 1
            if len(self.cases) > 0:
                print(f'{RED}Expected no errors, but got some{CLEAR}')
                return 1
            else:
                print(f'{GREEN}Errortest passed{CLEAR}')
                return 0
        with compiled_source as compiled:
            lib = yabo.YaboLib(compiled)
            for (test_name, pair) in self.cases.items():
                parser = lib.parser('test')
                buf = bytearray(pair.input)
                obj = parser.parse(buf)
                parsed_json = json.loads(pair.output)
                dict_obj = dictionarified_obj(obj)
                (diffed, is_different) = diff(parsed_json, dict_obj)
                if is_different:
                    print(f'{RED}Test {test_name} failed:{CLEAR}')
                    print(diffed.diff(), end='')
                    failed_tests += 1
                else:
                    print(f'{GREEN}Test {test_name} passed{CLEAR}')
        return failed_tests


# goes through all files in the target directory ending in .ybtest
def run_tests(target_dir: str) -> int:
    failed_tests = 0
    for file in os.listdir(target_dir):
        if not file.endswith('.ybtest'):
            continue
        print(f'Running test {file}')
        test = TestFile(open(os.path.join(target_dir, file)).read())
        failed_tests += test.run()
    return failed_tests


if not run_compiler_unit_tests():
    sys.exit(1)
target_folder = os.path.join(current_script_dir, 'tests')
failed_tests = run_tests(target_folder)
if failed_tests != 0:
    print(f'{failed_tests} tests failed')
    sys.exit(1)
else:
    print('All tests passed')
    sys.exit(0)
