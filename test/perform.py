#!/usr/bin/env python3

import os
import subprocess
import sys


EXE = '.exe' if sys.platform == 'win32' else ''
LIBC = '' #'-lmsvcrt' if sys.platform == 'win32' else ''


class TestMethod(object):
    def __init__(self, executable, timeout=None):
        self.executable = executable
        self.timeout = timeout
        self.stdin = None
        self.stdout = None
        self.returncode = None

    def __call__(self, args, input=None):
        # print(args)
        self.stdin = input
        try:
            self.completed = subprocess.run([self.executable] + args, timeout=self.timeout, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, stdin=self.stdin)
            self.stdout = self.completed.stdout
            self.returncode = self.completed.returncode
        except subprocess.TimeoutExpired as e:
            print('Running', self.executable, 'timed out after', self.timeout, 'seconds')
            return False
        return not self.wrong_returncode()

    def wrong_returncode(self, code=0):
        return self.returncode != code

    def wrong_output(self, expected=None):
        if expected is None:
            return False
        return expected != self.stdout

    def dump_output(self, filename, to_stdout=True, empty_too=False):
        print(">>>", self.__class__.__name__)
        if self.stdout is None:
            return
        if len(self.stdout) > 0 or empty_too:
            if to_stdout:
                log = str(self.stdout, 'utf-8', 'ignore')
                for line in log.splitlines():
                    print(line)
            if filename is None:
                return
            with open(filename, 'wb') as file:
                file.write(self.stdout)

    def load_source_file(self, filename):
        if filename is None:
            return None
        try:
            return open(filename, 'rb')
        except OSError as e:
            print('Cannot open source file', filename)
        return None


class RunImpalaCompile(TestMethod):
    def __call__(self, testfile):
        super().__call__(["-emit-llvm", "-O2", "-o", testfile.intermediate(), testfile.filename()])

        self.dump_output(testfile.intermediate('.log'))

        if self.wrong_returncode():
            print("Impala returned wrong returncode")
            return False

        expected_output = None
        logfilename = testfile.source('.log')
        if logfilename is not None:
            with open(logfilename, 'r') as logfile:
                expected_output = logfile.readlines()
        if self.wrong_output(expected_output):
            print("Impala generated invalid output")
            return False

        return True

class LinkFakeRuntime(TestMethod):
    def __init__(self, clang, runtime):
        super().__init__(clang)
        self.runtime = runtime

    def __call__(self, testfile):
        super().__call__([testfile.intermediate('.ll'), LIBC, self.runtime, "-o", testfile.intermediate(EXE)])

        self.dump_output(None)

        if self.wrong_returncode():
            print("Linking with", self.runtime, "failed.")
            return False

        return True

class ExecuteTestOutput(TestMethod):
    def __init__(self, timeout=None):
        super().__init__(None, timeout=timeout)

    def loadinput(self, testfile):
        return self.load_source_file(testfile.source('.in'))

    def __call__(self, testfile):
        if not os.path.isfile(testfile.intermediate(EXE)):
            return False
        self.executable = testfile.intermediate(EXE)
        stdin = self.loadinput(testfile)
        super().__call__([], input=stdin)

        self.dump_output(testfile.intermediate('.out'), to_stdout=False)

        if self.wrong_returncode():
            print("Executing output", testfile.filename(), "exited with non-zero returncode.")
            return False

        return True

class MultiStepPipeline(object):
    def __init__(self, *args):
        self.steps = args

    def __call__(self, testfile):
        for step in self.steps:
            if not step(testfile):
                return False
        return True


class TestFile(object):
    def __init__(self, filename, tempdir):
        self.temp = tempdir
        self.dir, basename = os.path.split(filename)
        self.base, self.ext = os.path.splitext(basename)

    def dirname(self):
        return os.path.join(self.temp, self.dir)

    def basename(self):
        return self.base

    def filename(self):
        return self.source(self.ext)

    def source(self, ext):
        filename = os.path.join(self.dir, self.base + ext)
        if os.path.isfile(filename):
            return filename
        return None

    def intermediate(self, ext=''):
        return os.path.join(self.temp, self.dir, self.base + ext)


def fetch_tokens(testfile, test_methods):
    firstline = testfile.readline()

    tokens = firstline.split()
    if len(tokens) < 1 or tokens[0] != "//":
        return None, None

    is_broken = "broken" in tokens

    procedure = list(set(tokens) & set(test_methods.keys()))
    if len(procedure) == 1:
        procedure = procedure[0]
    else:
        return None, is_broken

    return test_methods.get(procedure, None), is_broken

def search_in_path(executable):
    executable = executable + EXE
    for path in os.environ['PATH'].split(os.pathsep):
        path = path.strip('"')
        filename = os.path.join(path, executable)
        if os.path.isfile(filename) and os.access(filename, os.X_OK):
            print('Found', executable, 'at', filename)
            return filename
    return None

if __name__ == '__main__':
    import argparse
    import sys

    config = {'IMPALA_BIN': None, 'CLANG_BIN': None, 'TEMP_DIR': os.getcwd(), 'LIBRTMOCK': None}
    try:
        import configDebug as config
    except ImportError as e:
        pass
    try:
        import configRelease as config
    except ImportError as e:
        pass

    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('tests',                   nargs='*', help='path to one or multiple test files', type=argparse.FileType('r'))
    parser.add_argument('-i', '--impala',          nargs='?', help='path to impala',                     type=str, default=config.IMPALA_BIN)
    parser.add_argument('-c', '--clang',           nargs='?', help='path to clang',                      type=str, default=config.CLANG_BIN)
    parser.add_argument(      '--temp',            nargs='?', help='path to temp dir',                   type=str, default=config.TEMP_DIR)
    parser.add_argument('-l', '--rtmock',          nargs='?', help='path to rtmock',                     type=str, default=config.LIBRTMOCK)
    parser.add_argument('-t', '--compile-timeout', nargs='?', help='timeout for compiling test case',    type=int, default=5)
    parser.add_argument('-r', '--run-timeout',     nargs='?', help='timeout for running test case',      type=int, default=5)
    parser.add_argument('-p', '--pedantic',                   help='also run tests that are known to be broken or do not provide a valid testing procedure', action='store_true')
    args = parser.parse_args()

    if args.impala is None:
        args.impala = search_in_path('impala')

    if args.clang is None:
        args.clang = search_in_path('clang')

    if args.rtmock is None:
        print('Unable to determine the path to librtmock')
        sys.exit(2)

    PASSED = 0
    FAILED = 1
    SKIPPED = 77

    test_methods = {
        'codegen' : MultiStepPipeline(
            RunImpalaCompile(args.impala, timeout=args.compile_timeout),
            LinkFakeRuntime(args.clang, args.rtmock),
            ExecuteTestOutput(timeout=args.run_timeout)
        )
    }

    action = "Fail" if args.pedantic else "Skip"

    def handle_test(testfile):
        method, broken = fetch_tokens(testfile, test_methods)
        testfile.close()
        filename = testfile.name

        print("Testing", filename)

        if broken:
            print(action, "test", filename, "-", "The test is known to be broken.")
            return False if args.pedantic else None

        file = TestFile(filename, args.temp)
        if not os.path.isdir(file.dirname()):
            os.makedirs(file.dirname())

        if method is None:
            print(action, "test", filename, "-", "Unknown testing procedure!")
            return False if args.pedantic else None

        return method(file)

    result = None

    if len(args.tests) == 1:
        testfile = args.tests[0]
        result = handle_test(testfile)
    else:
        result = True
        for testfile in args.tests:
            success = handle_test(testfile)
            if success is None:
                success = not args.pedantic
            print('Test', testfile.name, 'passed.' if success else 'failed.')
            result &= success

    if result is None:
        print('SKIPPED')
        sys.exit(SKIPPED)

    if result:
        print('PASSED')
        sys.exit(PASSED)

    print('FAILED')
    sys.exit(FAILED)

