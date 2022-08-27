import os
import re
import tempfile
import subprocess

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

class CompiledSource:
    filename: str
    def __init__(self, source):
        file = tempfile.NamedTemporaryFile(delete=False)
        self.filename = file.name
        file.close()

    def __enter__(self):
        return self
    
    def __exit__(self, _exc_type, _exc_value, _traceback):
        os.remove(self.filename)

    

class InputOutputPair:
    input: bytes
    output: str
    def __init__(self, input, output):
        self.input = input
        self.output = output

class TestFile:
    source: str
    cases: dict[str, InputOutputPair]
    def __init__(self, text):
        self.parse(text)

    # parses a raw file into a TestFile object
    def parse(self, text):
        split = heading.split(text)
        binary_cases = {}
        output_cases = {}
        if split[1] != 'source':
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
            raise Exception('Extra output cases: ' + ', '.join(output_cases.keys()))

    def run(self):
        pass