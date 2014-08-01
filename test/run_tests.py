#!/usr/bin/env python

"""
This script can be used to execute the tests.

Usage: run_tests.py [options] [subdirectory that contains test cases]

Command line options:
 -e, --executable <Path to executable>
 -d, --disable-progressbar   Disable the fancy progress bar
 -t, --compiler-timeout <floating point value in seconds>
                         Default is 1.0
 -L, --valgrind   Use valgrind to check for memory leaks during testing
"""

import infrastructure.tests
from infrastructure.timed_process import CompileProcess
import os, sys, getopt

def invoke(executable, directory, pb, valgrind):
    tests = infrastructure.tests.get_tests_from_dir(directory)
    
    if valgrind:
        tests = [infrastructure.tests.ValgrindTest(t) for t in tests]
    
    infrastructure.tests.executeTests(tests, executable, pb)

def main():
    executable_unix = "../build/bin/impala"
    executable_windows = "../build/bin/Debug/impala.exe"
    if os.path.exists(executable_unix):
	    executable = executable_unix
    else:
	    executable = executable_windows
    pb = True
    valgrind = False
    
    # get cmd file
    try:
        opts, args = getopt.getopt(sys.argv[1:], "he:dt:L", ["help", "executable", "disable-progressbar", "compiler-timeout", "valgrind"])
    except getopt.error as msg:
        print(msg)
        sys.exit(2)
    
    # handle options
    for o, a in opts:
        if o in ("-h", "--help"):
            print(__doc__)
            sys.exit(0)
        if o in ("-e", "--executable"):
            executable = a
        if o in ("-d", "--disable-progressbar"):
            pb = False
        if o in ("-t", "--compiler-timeout"):
            CompileProcess.timeout = float(a)
        if o in ("-L", "--valgrind"):
            valgrind = True

    if len(args) > 1:
        print("You specified too many arguments.")
        print(__doc__)
        sys.exit(2)
    elif len(args) == 0:
        print("You did not specify a test directory. Using '.'")
        directory = "."
    else:
        directory = args[0]

    invoke(executable, directory, pb, valgrind)

main()
