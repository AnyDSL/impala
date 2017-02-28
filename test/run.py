#!/usr/bin/env python

import argparse
import enum
import math
import os
import subprocess
import sys

class Category(enum.Enum):
    codegen  = 0
    sema_pos = enum.auto()
    sema_neg = enum.auto()

def categorize(s):
    for cat in Category:
        if str(cat.name) == s:
            return cat.value
    return None

def is_exe(filename):
    return os.path.isfile(filename) and os.access(filename, os.X_OK)

def remove(filename):
    if os.access(filename, os.W_OK):
        os.remove(filename)

def find_impala():
    impala_exe = "impala.exe" if sys.platform == "win32" else "impala"

    for path in os.environ["PATH"].split(os.pathsep):
        path = path.strip('"')
        impala_path = os.path.join(path, impala_exe)
        if is_exe(impala_path):
            return impala_path

    return os.path.abspath(os.path.join("..", "build", "bin", impala_exe))

def classify(test):
    with open(test, 'r') as f:
        line = f.readline()
        if line.startswith("//"):
            return categorize(line[2:].split()[0])

def main():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('test',                    nargs='*', help='path to test or test directory',    default='.',           type=str)
    parser.add_argument('-i', '--impala',          nargs='?', help='path to impala',                    default=find_impala(), type=str)
    parser.add_argument('-c', '--compile-timeout', nargs='?', help='timeout for compiling test case',   default=5,             type=int)
    parser.add_argument('-r', '--run-timeout',     nargs='?', help='timeout for running test case',     default=5,             type=int)
    parser.add_argument('-n', '--nocleanup',                  help='don\'t clean up temporary files',   action='store_true')
    parser.add_argument('-b', '--broken',                     help='also run tests known to be broken', action='store_true')
    args = parser.parse_args()

    if not os.access("lib.o", os.R_OK):
        print(">>> building lib.o")
        subprocess.run(["clang", "-c", "-O2", "infrastructure/lib.c"])

    tests = [[], [], []]

    for test in args.test:
        if os.path.isfile(test):
            tests.append(test)
        else:
            for dirpath, dirs, files in os.walk(test):
                for filename in files:
                    if os.path.splitext(filename)[1] == ".impala":
                        path = os.path.join(dirpath,filename)
                        c = classify(path)
                        if c != None:
                            tests[c].append(path)

    total = sum([len(l) for l in tests])
    align = int(math.log10(total)) + 1
    i = 1
    for cat in tests:
        for test in cat:
            base = os.path.splitext(os.path.split(test)[1])[0]
            test_ll =  base + ".ll"

            def impala(flags):
                try:
                    subprocess.run([args.impala] + flags + [test], timeout=args.compile_timeout)
                except subprocess.TimeoutExpired as timeout:
                    print("!!! '{}' timed out after {} seconds".format(timeout.cmd, timeout.timeout))

            def link():
                subprocess.run(["clang", "-s", test_ll, "lib.o", "-o", base])

            def run():
                try:
                    subprocess.run(["base"], args.run-timeout)
                except subprocess.TimeoutExpired as timeout:
                    print("!!! '{}' timed out after {} seconds".format(timeout.cmd, timeout.timeout))

            print((">>> [{:>%i}/{}] {}" % align).format(i, total, test))

            classify(test)
            # if has_main(test):
            if False:
                impala(["-emit-llvm", "-O2"])
                link()
            else:
                pass
                # impala([])

            if not args.nocleanup:
                remove(test_ll)
                remove(base)

            i = i + 1

sys.exit(main())
