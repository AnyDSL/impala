#!/usr/bin/env python

import argparse
import math
import os
import subprocess
import sys

def is_exe(filename):
    return os.path.isfile(filename) and os.access(filename, os.X_OK)

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
    args = parser.parse_args()

    def impala(flags, prg):
        subprocess.run([args.impala] + flags + [prg], timeout=args.compile_timeout)

    tests = [];
    for dirpath, dirs, files in os.walk(args.test): 
        for filename in files:
            tests.append(os.path.join(dirpath,filename))

    i = 0
    align = int(math.log10(len(tests)))
    for test in tests:
        print(">>> [{:>3}/{}]".format(i, len(tests)))
        impala([], test)
        i = i + 1
    print(tests)
    #impala(["-emit-llvm"], "codegen/endless_mangling.impala")
    #parser.print_help()

sys.exit(main())
