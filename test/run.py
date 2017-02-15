#!/usr/bin/env python

import argparse
import os
import sys

def is_exe(file):
    return os.path.isfile(file) and os.access(file, os.X_OK)

def find_impala():
    impala_exe = "impala.exe" if sys.platform == "win32" else "impala"

    for path in os.environ["PATH"].split(os.pathsep):
        path = path.strip('"')
        impala_path = os.path.join(path, impala_exe)
        if is_exe(impala_path):
            return impala_path

    return os.path.abspath(os.path.join("..", "build", "bin", impala_exe))

def main():
    exe = find_impala()

    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('test',                    nargs='?', help='path to test or test directory',  default='.', type=str)
    parser.add_argument('-e', '--executable',      nargs='?', help='path to impala executable',       default=exe, type=str)
    parser.add_argument('-c', '--compile-timeout', nargs='?', help='timeout for compiling test case', default=6,   type=int)
    parser.add_argument('-r', '--run-timeout',     nargs='?', help='timeout for running test case',   default=6,   type=int)
    args = parser.parse_args()
    print(args.executable)
    print(args.compile_timeout)
    print(args.run_timeout)
    #parser.print_help()
    enum_tests()

def enum_tests():
    pass
    # for dirName, subdirList, fileList in os.walk(rootDir):
    # print('Found directory: %s' % dirName)
    # for fname in fileList:
    # print(fname)
    # print('\t%s' % fname)

sys.exit(main())
