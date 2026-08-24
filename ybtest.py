#!/usr/bin/env python3
# Test runner for yabo

from collections import defaultdict
import copy
from dataclasses import dataclass
import os
import pathlib
import sys
import shutil
import re
import tempfile
import subprocess
import json
import time
from typing import Optional, Tuple, Any, override
from concurrent import futures
import yabo

TARGET_RELEASE = 'debug'
BINARY_NAME = 'yaboc'
yabo.MASK_TEST_MODE = True

# test files are structured as follows:
# ====[ source code ]====
# (source)
# ====[ binary 1 ]====
# (hexdump of input)
# ====[ output 1 ]====
# (json of output)
#
# this regex will match the title lines to split it
heading = re.compile(r'^={3,}\[\s*(.*?)\s*\]\={3,}$', re.MULTILINE)

# this regex matches a input/output title
case_title = re.compile(r'^(binary|text|output)\s+(.+)$')

# a comment that contains the error message
# for example, '#~^ error[301] not found here'
# represents an error message that should be
# one line above the line with the comment
# with error code 301
error_comment = re.compile(r'^.*#(~\^*)\s*error\[(\d+)\]\s*(.*)$')

current_script_dir = pathlib.Path(os.path.dirname(os.path.realpath(__file__)))
core_path = current_script_dir / 'lib' / 'core.yb'
lib_path = current_script_dir / 'lib'
example_path = current_script_dir / 'examples'
test_order_path = current_script_dir / 'test_order.txt'
compiler_env = os.environ.copy()
compiler_env['YABO_LIB_PATH'] = str(lib_path)
compiler_env['RUST_BACKTRACE'] = '1'
# filter out LD_PRELOAD from the environment
compiler_env['LD_PRELOAD'] = ''
compiler_dir = current_script_dir / 'crates' / 'yaboc'
compiler_bin = "yaboc"


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
        for msg in diagnostic['labels']:
            line: int = msg['start_line']
            message: str = f'{diagnostic["message"]}, {msg["message"]}'
            ret[line].append(ErrorLocation(message, line, code))
        return ret

    def is_contained_in(self, other: 'ErrorLocation') -> bool:
        return (
            self.contained_message in other.contained_message and
            self.code == other.code and
            self.line == other.line
        )


class TmpFile:
    filename: str | None
    def __init__(self, suffix: str = "", content: bytes | None = None):
        fd, name = tempfile.mkstemp(suffix=suffix)
        if content:
            os.write(fd, content)
        os.close(fd)
        self.filename = name

    def __enter__(self) -> "TmpFile":
        if not self.filename:
            raise ValueError("Tmp file already moved")
        return self

    def __exit__(self, _exc_type, _exc_value, _traceback):
        self.close()

    def move(self) -> "TmpFile":
        if not self.filename:
            raise ValueError("Tmp file already moved")
        ret = copy.copy(self)
        self.filename = None
        return ret

    def name(self) -> str:
        if not self.filename:
            raise ValueError("Tmp file already moved")
        return self.filename

    def close(self):
        if self.filename:
            os.remove(self.filename)
            self.filename = None


def build_compiler_binary():
    os.chdir(compiler_dir)
    with subprocess.Popen(
            ['cargo', 'metadata', '--format-version=1'], stdout=subprocess.PIPE, env=compiler_env) as cargo_metadata:
        cargo_metadata_output = json.loads(
            cargo_metadata.communicate()[0].decode('utf-8'))
        cargo_args = ['cargo', 'build']
        if TARGET_RELEASE == 'release':
            cargo_args.append('--release')
        subprocess.run(cargo_args, check=True, env=compiler_env)
        return os.path.join(cargo_metadata_output['target_directory'], TARGET_RELEASE, BINARY_NAME)


def run_compiler_unit_tests():
    os.chdir(compiler_dir)
    if shutil.which('cargo-nextest'):
        cargo_args = ['cargo', 'nextest', 'run']
    else:
        cargo_args = ['cargo', 'test']
    cargo_args.append('--workspace')
    if TARGET_RELEASE == 'release':
        cargo_args.append('--release')
    cargo_test = subprocess.run(cargo_args, check=False, env=compiler_env)
    return cargo_test.returncode == 0

def check_diagnostic_match(diagnostics: dict[int, list[ErrorLocation]], expected: ErrorLocation):
    if expected.line not in diagnostics or len(diagnostics[expected.line]) == 0:
        raise Exception(
            f'Expected error {expected.code} on line {expected.line}, '
            'but no error was found'
        )
    line_diagnostics = diagnostics[expected.line]
    found = any(expected.is_contained_in(actual)
                for actual in line_diagnostics)
    if not found:
        if len(line_diagnostics) == 1:
            diag = line_diagnostics[0]
            if diag.code != expected.code:
                raise Exception(
                    f'Expected error {expected.code} on line {expected.line}, '
                    f'but found error {diag.code} instead'
                )
            else:
                raise Exception(
                    f'Expected error message "{expected.contained_message}" on '
                    f'line {expected.line}, but found "{diag.contained_message}"'
                )
        raise Exception(
            f'Expected error {expected.code} on line {expected.line}, '
            'but none of the errors matched'
        )

def expected_errors(source: str) -> list[ErrorLocation]:
    expected: list[ErrorLocation] = []
    for (linenum, current_line) in enumerate(source.splitlines()):
        linenum = linenum + 1
        match = error_comment.match(current_line)
        if not match:
            continue
        expected += [ErrorLocation.from_match(match, linenum)]

    return expected

def check_errors(stderr: str, expected: list[ErrorLocation]) -> None:
    diagnostics: dict[int, list[ErrorLocation]] = defaultdict(list)
    for error in stderr.splitlines():
        try:
            error_json = json.loads(error)
        except json.JSONDecodeError as e:
            raise Exception(f'Error decoding {stderr}: {e}')
        for (line, errors) in ErrorLocation.from_diagnostics(error_json).items():
            diagnostics[line].extend(errors)

    if len(expected) == 0:
        raise Exception(
            f'No error comments found in source but errors were found in stderr:\n{stderr}'
        )

class Runner:
    def run(self, _input: bytes) -> Any:
        raise NotImplementedError("run not implemented")

    #def is_same(self, _other: Runner) -> bool:
    #    raise NotImplementedError("is_same not implemented")

    def __enter__(self) -> "Runner":
        return self

    def __exit__(self, _exc_type, _exc_value, _traceback):
        pass

class CompilerError(Exception):
    diagnostics: str

    def __init__(self, diagnostics: str):
        super().__init__(diagnostics)
        self.diagnostics = diagnostics

class Platform:
    def name(self) -> str:
        return "Abstract"

    def compile(self, _source_path: str, _source_name: str) -> TmpFile:
        raise NotImplementedError("compile not implemented")

    def create_runner_for_file(self, _source_path: str, _source_name: str) -> Runner:
        raise NotImplementedError("create_runner_for not implemented")

    def create_runner_for_source(self, source: str, source_name: str) -> Runner:
        with TmpFile(content=source.encode(), suffix=f"{source_name}.yb") as file:
            return self.create_runner_for_file(file.name(), source_name)

    def expect_compile_error_for(self, source: str, source_name: str, expected: list[ErrorLocation]) -> None:
        try:
            with self.create_runner_for_source(source, source_name):
                raise Exception("Expected compiler error but no errors were found")
        except CompilerError as e:
            check_errors(e.diagnostics, expected)

    def __enter__(self) -> "Platform":
        return self

    def __exit__(self, _exc_type, _exc_value, _traceback):
        pass


@dataclass
class InputOutputPair:
    input: bytes
    output: str


def dictionarified_obj(obj) -> Any:
    if isinstance(obj, (yabo.NominalValue, yabo.U8Value)):
        obj = obj.deref()
    if isinstance(obj, (int, str, bool)):
        return obj
    if isinstance(obj, yabo.ArrayValue):
        return [dictionarified_obj(obj[i]) for i in range(len(obj))]
    if isinstance(obj, yabo.ParserValue):
        return "parser"
    if isinstance(obj, yabo.FunArgValue):
        return "fun_args"
    if isinstance(obj, yabo.UnitValue):
        return "unit"
    if isinstance(obj, yabo.BlockValue):
        ret_dict = {}
        for field in obj.fields():
            ret_dict[field] = dictionarified_obj(obj.get(field))
        return ret_dict


def wrap_maybe_field(inner: str, indent: str, field: Optional[str] | list = None):
    if isinstance(field, list):
        return f'{indent}{inner},\n'
    if field is None:
        return inner + '\n'
    return f'{indent}"{field}": {inner},\n'


def dict_with_indent(d, indent: str, field=None) -> str:
    if isinstance(d, str):
        ret = f'"{d}"'
    elif isinstance(d, int):
        ret = str(d)
    elif isinstance(d, bool):
        if d:
            ret = 'true'
        else:
            ret = 'false'
    elif isinstance(d, list):
        ret = '[\n'
        for item in d:
            ret += dict_with_indent(item, indent + '  ', field=[])
        ret += f'{indent}]'
    elif not isinstance(d, dict):
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
    data: dict[str, Any]

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


class ListHead:
    __slot__ = ['data']
    data: list[Any]

    def __init__(self, data):
        self.data = data

    def diff(self, indent='', field=None) -> str:
        out = '[\n'
        for i, value in enumerate(self.data):
            out += value.diff(indent + '  ', field=[])
        out += f'{indent}]'
        return wrap_maybe_field(out, indent, field)


def diff(left: Any, right: Any) -> Tuple[MatchingHead | DiffHead | DictHead | ListHead, bool]:
    if isinstance(left, dict) and isinstance(right, dict):
        is_different = False
        ret = {}
        both_fields = set(left.keys()) | set(right.keys())
        for field in both_fields:
            left_field = left.get(field)
            right_field = right.get(field)
            (ret[field], is_field_diff) = diff(left_field, right_field)
            is_different |= is_field_diff
        return (DictHead(ret), is_different)

    if isinstance(left, list) and isinstance(right, list):
        is_different = False
        max_len = max(len(left), len(right))
        ret_list = []
        for i in range(max_len):
            left_field = left[i] if i < len(left) else None
            right_field = right[i] if i < len(right) else None
            (list_element, is_field_diff) = diff(left_field, right_field)
            ret_list.append(list_element)
            is_different |= is_field_diff
        return (ListHead(ret_list), is_different)

    if left != right:
        return (DiffHead(left, right), True)

    return (MatchingHead(left), False)

class NativeRunner(Runner):
    libfile: TmpFile

    def __init__(self, libfile: TmpFile):
        self.libfile = libfile

    @override
    def run(self, input: bytes) -> Any:
        buf = bytearray(input)
        lib = yabo.YaboLib(self.libfile.name(), buf, autoderef=False)
        obj = lib.parser('test').parse(buf)
        dict_obj = dictionarified_obj(obj)
        return dict_obj

class Native(Platform):
    def __init__(self):
        pass

    @override
    def name(self) -> str:
        return "Native"

    @override
    def compile(self, source_path: str, source_name: str) -> TmpFile:
        with TmpFile(suffix=f".{source_name}.so") as libfile:
            proc = subprocess.run(
                [compiler_bin, "--output-json", "--module", f"core={core_path}", source_path, libfile.name()],
                stderr=subprocess.PIPE,
                env=compiler_env
            )
            if proc.returncode != 0:
                raise CompilerError(proc.stderr.decode())

            return libfile.move()

    @override
    def create_runner_for_file(self, source_path: str, source_name: str) -> NativeRunner:
        with self.compile(source_path, source_name) as libfile:
            return NativeRunner(libfile.move())


class WasmRunner(Runner):
    exec: TmpFile

    def __init__(self, exec: TmpFile):
        self.exec = exec

    @override
    def run(self, input: bytes) -> Any:
        with tempfile.TemporaryDirectory() as tmpdir:
            inputpath = os.path.join(tmpdir, 'input')
            with open(inputpath, 'wb') as inputfile:
                inputfile.write(input)
            proc = subprocess.run(['wasmtime', '--dir', tmpdir, '--wasm=tail-call',
                                   self.exec.name(), inputpath],
                                  stdout=subprocess.PIPE)
            return json.loads(proc.stdout.decode('utf-8'))

    @override
    def __exit__(self, _exc_type, _exc_value, _traceback):
        self.exec.close()
        super().__exit__(_exc_type, _exc_value, _traceback)


class Wasm(Platform):
    cc: str
    sysroot: str
    printer: TmpFile

    def __init__(self, wasi_sdk_path: str):
        self.cc = os.path.join(wasi_sdk_path, 'bin', 'clang')
        self.sysroot = os.path.join(wasi_sdk_path, 'share', 'wasi-sysroot')
        self.printer = TmpFile(suffix='.yaboprint.o')
        compiler_args = [
            self.cc, '--sysroot', self.sysroot, '-c',
            '-DSTATIC_PARSER=test',
            '-I', str(current_script_dir / 'include'),
            '-D_WASI_EMULATED_MMAN',
            '-o', self.printer.name(),
            str(current_script_dir / 'tools' / 'yaboprint' / 'yaboprint.c')
        ]
        subprocess.run(compiler_args, check=True)

    @override
    def name(self) -> str:
        return "Wasm"

    @override
    def compile(self, source_path: str, source_name: str) -> TmpFile:
        with TmpFile(suffix=f'.{source_name}.o') as object:
            proc = subprocess.run(
                [compiler_bin, "--output-json",
                    "--target=wasm32-wasi", "--emit=object",
                    "--target-features=+tail-call",
                    "--module", f"core={core_path}",
                    source_path, object.name()],
                stderr=subprocess.PIPE,
                env=compiler_env,
            )
            if proc.returncode != 0:
                raise CompilerError(proc.stderr.decode())
            return object.move()

    @override
    def create_runner_for_file(self, source_path: str, source_name: str) -> WasmRunner:
        with (self.compile(source_path, source_name) as object,
              TmpFile(suffix=f'.{source_name}') as execpath):
            clang_args = [
                self.cc, '--sysroot', self.sysroot,
                '-lwasi-emulated-mman',
                '-Wl,-z,stack-size=10000000',
                '-o', execpath.name(), self.printer.name(), object.name()
            ]
            subprocess.run(clang_args, check=True)
            return WasmRunner(execpath.move())

    @override
    def __exit__(self, _exc_type, _exc_value, _traceback):
        self.printer.close()
        super().__exit__(_exc_type, _exc_value, _traceback)


class TestFile:
    source: str
    name: str
    cases: dict[str, InputOutputPair]

    def __init__(self, source: str, name: str):
        self.cases = {}
        self.source = source
        self.name = name
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
                if test_name in binary_cases:
                    raise Exception('Duplicate binary name: ' + test_name)
                binary_cases[test_name] = bytes.fromhex(subtext)
            elif kind == 'text':
                if test_name in binary_cases:
                    raise Exception('Duplicate binary name: ' + test_name)
                binary_cases[test_name] = subtext.lstrip().encode('utf-8')
            elif kind == 'output':
                if test_name in output_cases:
                    raise Exception('Duplicate output name: ' + test_name)
                output_cases[test_name] = subtext

        for (test_name, binary) in binary_cases.items():
            try:
                output = output_cases.pop(test_name)
            except KeyError as e:
                raise Exception('No output for test case: ' + test_name) from e
            self.cases[test_name] = InputOutputPair(binary, output)

        if output_cases:
            raise Exception('Extra output cases: ' +
                            ', '.join(output_cases.keys()))

    def check_errors(self, platform: Platform, expected: list[ErrorLocation], output: str) -> int:
        try:
            platform.expect_compile_error_for(self.source, self.name, expected)
        except Exception as e:
            output += f'{RED} {e}{CLEAR}'
            print(output)
            return 1
        if self.cases:
            output += f'{RED} Expected no errors, but got some{CLEAR}'
            print(output)
            return 1
        output += f'{GREEN} Errortest passed{CLEAR}'
        print(output)
        return 0

    def run(self, platforms: list[Platform]) -> int:
        output: str = f'Running test {self.name}\n'
        failed_tests = 0
        expected_errs = expected_errors(self.source)
        if expected_errs:
            return self.check_errors(platforms[0], expected_errs, output)
        for platform in platforms:
            with platform.create_runner_for_source(self.source, self.name) as runner:
                for (test_name, pair) in self.cases.items():
                    try:
                        parsed_output = runner.run(pair.input)
                    except Exception as e:
                        output += f'{RED} {platform.name()} Test {test_name} failed:{CLEAR}\n'
                        output += f'{RED} {e}{CLEAR}\n'
                        failed_tests += 1
                        continue
                    parsed_json = json.loads(pair.output)
                    (diffed, is_different) = diff(parsed_json, parsed_output)
                    if is_different:
                        output += f'{RED} {platform.name()} Test {test_name} failed:{CLEAR}\n'
                        output += diffed.diff()
                        failed_tests += 1
                    else:
                        output += f'{GREEN} {platform.name()} Test {test_name} passed{CLEAR}\n'


        print(output, end='')
        return failed_tests


def run_test(spath: str, platforms: list[Platform]) -> tuple[int, float]:
    path = pathlib.Path(spath)
    if path.suffix != '.ybtest':
        return (0, 0)
    with open(path, 'r', encoding='utf-8') as file:
        content = file.read()
        starttime = time.time()
        testname = os.path.basename(path).removesuffix('.ybtest')
        test = TestFile(content, testname)
        count = test.run(platforms)
        endtime = time.time()
        elapsed = (endtime - starttime)
        return (count, elapsed)

def get_test_order_list(paths: list[str]) -> list[str]:
    try:
        with open(test_order_path, 'r') as file:
            pathset = set(paths)
            order = file.read().splitlines()
            existing_paths = [path for path in order if path in pathset]
            missing_paths = pathset - set(existing_paths)
            return existing_paths + list(missing_paths)
    except FileNotFoundError:
        return paths

def write_test_order_file(times: dict[str, float]):
    increasing = sorted((exec_time, path) for (path, exec_time) in times.items())
    paths = [path for (_, path) in increasing][::-1]
    with open(test_order_path, 'w') as outfile:
        outfile.write('\n'.join(paths))

def run_test_with_path_returned(file: str, platforms: list[Platform]) -> tuple[str, int, float]:
    return (file, *run_test(file, platforms))

# goes through all files in the target directory ending in .ybtest
def run_tests(files: list[str], platforms: list[Platform], collect: bool = True) -> int:
    try:
        with futures.ThreadPoolExecutor() as executor:
            results = executor.map(lambda file: run_test_with_path_returned(file, platforms), files)
            sum = 0
            times = dict()
            for (file, count, time) in results:
                times[str(file)] = time
                sum += count

            if collect and sum == 0:
                write_test_order_file(times)

            return sum
    except ValueError as e:
        print(f'Encountered error ({e}), running tests sequentially')
        total_failed = 0
        for file in files:
            print(f'Running {file}')
            total_failed += run_test(file, platforms)[0]
        return total_failed


def compile_example(file: os.PathLike[str], platforms: list[Platform]):
    output: str = f'Compiling example {file}\n'
    failed: int = 0
    name = os.path.basename(file)
    for platform in platforms:
        try:
            with platform.compile(str(file), name):
                output += f'{GREEN} {platform.name()} compilation passed{CLEAR}\n'
        except CompilerError as e:
            output += f'{RED} {platform.name()} compilation failed{CLEAR}\n'
            output += e.diagnostics
            failed += 1

    print(output, end='')
    return failed


def compile_examples(platforms: list[Platform]):
    total_failed = 0
    files = [example_path /
             x for x in os.listdir(example_path) if x.endswith('.yb')]
    with futures.ThreadPoolExecutor() as executor:
        results = executor.map(lambda file: compile_example(file, platforms), files)
        total_failed = sum(results)
    return total_failed


def main(args):
    global compiler_bin
    arg_list = [os.path.abspath(file) for file in args]
    compiler_bin = build_compiler_binary()
    platforms: list[Platform] = [Native()]
    wasi_sdk_path = os.environ.get('WASI_SDK_PATH')
    if wasi_sdk_path:
        platforms += [Wasm(wasi_sdk_path)]
    if len(arg_list) == 0:
        if not run_compiler_unit_tests():
            sys.exit(1)
        target_dir = current_script_dir / 'tests'
        files = [str(target_dir / x) for x in os.listdir(target_dir)]
        files = get_test_order_list(files)
        with futures.ThreadPoolExecutor() as executor:
            tests = executor.submit(run_tests, files, platforms)
            compile = executor.submit(compile_examples, platforms)
            total_failed = tests.result() + compile.result()
    else:
        total_failed = run_tests(arg_list, platforms, collect=False)

    if total_failed != 0:
        print(f'{total_failed} tests failed')
        sys.exit(1)
    else:
        print('All tests passed')
        sys.exit(0)


if __name__ == '__main__':
    main(sys.argv[1:])
