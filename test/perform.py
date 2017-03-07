#!/usr/bin/env python

# import math
import os
import subprocess

categories = ["codegen", "sema_pos", "sema_neg"];

def launch(args, expected_code=0, input=None, expected_output=None, timeout=None):
    print("Launch:", args)
    try:
        stdout = subprocess.PIPE if expected_output is not None else None
        completed = subprocess.run(args, timeout=timeout, stdout=stdout, stderr=subprocess.STDOUT, universal_newlines=True)
        return completed
    except subprocess.TimeoutExpired as timeout:
        print("!!! '{}' timed out after {} seconds".format(timeout.cmd, timeout.timeout))
    return None

def run_impala(testfile, **kwargs):
    impala = kwargs.get("impala", "impala")
    timeout = kwargs.get("timeout", None)
    flags = ["-emit-llvm", "-O2", "-o", testfile.intermediate()]
    result = launch([impala] + flags + [testfile.filename()], timeout=timeout)
    print(result.stdout)
    if result.returncode != 0:
        return False
    logfile = testfile.source(".log")
    if logfile:
        # TODO: diff output log
        pass
    return True

def run_link(testfile, **kwargs):
    clang = kwargs.get("clang", "clang")
    libc = kwargs.get("libc", "testlibc.lib")
    flags = [testfile.intermediate('.ll'), libc, "-o", testfile.intermediate('.exe')]
    result = launch([clang] + flags)
    print(result.stdout)
    return result.returncode == 0

def run_test(testfile, **kwargs):
    timeout = kwargs.get("timeout", None)
    result = launch([testfile.intermediate('.exe')], timeout=timeout)
    print(result.stdout)
    return result.returncode == 0
    # try:
        # out = open(test_out_new, 'w')
        # input = open(test_in, 'r') if os.access(test_in, os.R_OK) else None
        # return subprocess.run([test_exe], args.run_timeout, stdin=input, stdout=out, stderr=out, encoding="utf-8").returncode == 0
    # except subprocess.TimeoutExpired as timeout:
        # print("!!! '{}' timed out after {} seconds".format(timeout.cmd, timeout.timeout))
        # return False



class TestFile(object):
    def __init__(self, filename, **flags):
        self.temp = flags.get('temp', os.getcwd())
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

def test_codegen(filename, flags):
    print("Testing", filename)

    file = TestFile(filename, **flags)
    if not os.path.isdir(file.dirname()):
        os.makedirs(file.dirname())

    if not run_impala(file, **flags):
        return False

    if not run_link(file, **flags):
        return False

    if not run_test(file, **flags):
        return False

    return True

def fetch_tokens(testfile):
    firstline = testfile.readline()

    tokens = firstline.split()
    if len(tokens) < 1 or tokens[0] != "//":
        return None, None

    is_broken = "broken" in tokens

    procedure = list(set(tokens) & set(categories))
    if len(procedure) == 1:
        procedure = procedure[0]
    else:
        procedure = None

    if procedure == "codegen":
        return test_codegen, is_broken

    return None, is_broken

def handle_testcase(testfile, flags, pedantic=False):
    action = "Fail" if pedantic else "Skip"

    test_proc, broken = fetch_tokens(testfile)
    testfile.close()
    filename = testfile.name

    if test_proc is None:
        print(action, "test", filename, "-", "Unknown testing procedure!")
        return not pedantic

    if broken:
        print(action, "test", filename, "-", "The test is known to be broken.")
        return not pedantic

    return test_proc(filename, flags)

if __name__ == '__main__':
    import argparse
    import sys

    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('tests',           nargs='*', help='path to test or test directory', type=argparse.FileType('r'))
    parser.add_argument('-i', '--impala',  help='path to impala',                    required=True, type=str)
    parser.add_argument('-c', '--clang',   help='path to clang',                     required=True, type=str)
    parser.add_argument('-t', '--temp',    help='path to temp dir',                  required=True, type=str)
    parser.add_argument('-l', '--libc',    help='path to testlibc',                  required=True, type=str)
    # parser.add_argument('-c', '--compile-timeout', nargs='?', help='timeout for compiling test case',   default=5,             type=int)
    # parser.add_argument('-r', '--run-timeout',     nargs='?', help='timeout for running test case',     default=5,             type=int)
    # parser.add_argument('-n', '--nocleanup',                  help='don\'t clean up temporary files',   action='store_true')
    parser.add_argument('-p', '--pedantic',                     help='also run tests that are known to be broken or do not provide a valid testing procedure', action='store_true')
    args = parser.parse_args()

    success = True

    flags = {
        'impala': args.impala,
        'clang': args.clang,
        'timeout': 5,
        'temp': args.temp,
        'libc': args.libc
    }

    for testfile in args.tests:
        success &= handle_testcase(testfile, flags, args.pedantic)

    if not success:
        sys.exit(1)

    sys.exit(0)
