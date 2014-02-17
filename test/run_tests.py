"""
This script can be used to execute the tests. 

Call it with:
 -e <Path to you executable>
 -g either gruppe42 or cc-1 to execute the tests by gruppe42/cc-1 respectively. 
 
To add your own tests, create a tests.py in your group directory, after that, -g <your group> should work. 
Have a look at gruppe42/tests.py to see how to write a tests.py file. 
"""

import infrastructure.tests
import os, sys, getopt, imp

def invoke(executable, group, pb):
    tests = imp.load_source("tests", os.path.join(group, "tests.py"))
    infrastructure.tests.executeTests(tests.allTests(), executable, pb)

def main():
    pb = True
    # get cmd file
    try:
        opts, args = getopt.getopt(sys.argv[1:], "he:g:", ["help", "executable", "group","disable-progressbar"])
    except getopt.error as msg:
        print(msg)
        sys.exit(2)
    # handle options
    for o, a in opts:
        if o in ("-h", "--help"):
            print(__doc__)
            sys.exit(0)
        if o in ("-g", "--group"):
            group = a
        if o in ("-e", "--executable"):
            executable = a
        if o in ("-p", "--prepared"):
            p = True
        if o in ("--disable-progressbar"):
            pb = False

    if not 'group' in locals():
        print("You did not specify the -g option.")
        print(__doc__)
    elif not 'executable' in locals():
        print("You did not specify the -e option.")
        print(__doc__)
    else:
        invoke(executable, group, pb)

main()
