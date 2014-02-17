#!/usr/bin/env python

"""
This script can be used to execute the tests.

Command line options:
 -e, --executable <Path to executable>
 -d, --directory <subdirectory that contains a group of tests>  (Mandatory)
 --disable-progressbar Disable the fancy progress bar
"""

import infrastructure.tests
import os, sys, getopt, imp

def invoke(executable, directory, pb):
    tests = imp.load_source("tests", os.path.join(directory, "tests.py"))
    infrastructure.tests.executeTests(tests.allTests(), executable, pb)

def main():
    executable = "../build/bin/impala"
    pb = True
    # get cmd file
    try:
        opts, args = getopt.getopt(sys.argv[1:], "he:d:", ["help", "executable", "directory","disable-progressbar"])
    except getopt.error as msg:
        print(msg)
        sys.exit(2)
    # handle options
    for o, a in opts:
        if o in ("-h", "--help"):
            print(__doc__)
            sys.exit(0)
        if o in ("-d", "--directory"):
            directory = a
        if o in ("-e", "--executable"):
            executable = a
        if o in ("--disable-progressbar"):
            pb = False

    if not 'directory' in locals():
        print("You did not specify the -d option.")
        print(__doc__)
    else:
        invoke(executable, directory, pb)

main()
