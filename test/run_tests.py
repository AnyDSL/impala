#!/usr/bin/env python

"""Usage: run_tests.py [options] [subdirectory that contains test cases]

If no subdirectory is given '.' is assumed.

Command line options:
 -e, --executable <Path to executable>
     Default is 'impala' (if possible) or '../build/bin/impala' otherwise;
     on Windows '.exe' is appended
 -t, --compiler-timeout <floating point value in seconds>
                         Default is 5.0
 -r, --runtime-timeout <floating point value in seconds>
                         Default is 5.0
 -L, --valgrind   Use valgrind to check for memory leaks during testing
"""

import infrastructure.tests
from infrastructure.timed_process import CompileProcess
import os, sys, getopt, subprocess

def invoke(executable, dir_or_file, valgrind):
    print("Using '%s' as executable." % executable)
    
    if(os.path.isfile(dir_or_file)):
        tests = infrastructure.tests.get_tests_for_file(dir_or_file)
    else:
        tests = infrastructure.tests.get_tests_from_dir(dir_or_file)
   
    if valgrind:
        tests = [infrastructure.tests.ValgrindTest(t) for t in tests]
    
    return infrastructure.tests.executeTests(tests, executable)

def get_executable():
    if sys.platform == "win32":
        executable = "impala.exe"
    else:
        executable = "impala"

    # first check if impala is in $PATH
    def is_exe(fpath):
        return os.path.isfile(fpath) and os.access(fpath, os.X_OK)

    fpath, fname = os.path.split(executable)
    if fpath:
        if is_exe(executable):
            return executable
    else:
        for path in os.environ["PATH"].split(os.pathsep):
            path = path.strip('"')
            exe_file = os.path.join(path, executable)
            if is_exe(exe_file):
                return os.path.abspath(exe_file)

    return os.path.abspath(os.path.join("..", "build", "bin", executable))

def main():
    executable = get_executable()
    valgrind = False
    
    # get cmd file
    try:
        opts, args = getopt.getopt(sys.argv[1:], "he:t:L", ["help", "executable", "compiler-timeout", "runtime-timeout", "valgrind"])
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
        if o in ("-t", "--compiler-timeout"):
            CompileProcess.timeout = float(a)
        if o in ("-r", "--runtime-timeout"):
            RuntimeProcess.timeout = float(a)
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

    return invoke(executable, directory, valgrind)

sys.exit(main())
