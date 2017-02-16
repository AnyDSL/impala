#!/usr/bin/env python

import argparse
import math
import os
import subprocess
import sys

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

def main():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('test',                    nargs='?', help='path to test or test directory',  default='.',           type=str)
    parser.add_argument('-i', '--impala',          nargs='?', help='path to impala',                  default=find_impala(), type=str)
    parser.add_argument('-c', '--compile-timeout', nargs='?', help='timeout for compiling test case', default=5,             type=int)
    parser.add_argument('-r', '--run-timeout',     nargs='?', help='timeout for running test case',   default=5,             type=int)
    parser.add_argument('-n', '--nocleanup',                  help='don\'t clean up temporary files', action='store_true')
    args = parser.parse_args()

    if not os.access("lib.o", os.R_OK):
        print(">>> building lib.o")
        subprocess.run(["clang", "-c", "-O2", "infrastructure/lib.c"])

    tests = [];
    for dirpath, dirs, files in os.walk(args.test): 
        for filename in files:
            if os.path.splitext(filename)[1] == ".impala":
                tests.append(os.path.join(dirpath,filename))

    i = 0
    align = int(math.log10(len(tests)))
    for test in tests:
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

        print(">>> [{:>3}/{}] {}".format(i, len(tests), test))

        impala(["-emit-llvm"])
        link()
        if not args.nocleanup:
            remove(test_ll)
            remove(base)

        i = i + 1

sys.exit(main())
