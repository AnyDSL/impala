"""
tests.py for semantic analysis tests
"""

import os

# import the test infrastructure
from infrastructure.tests import InvokeTest

POS_DIR = "sema/positive"
NEG_DIR = "sema/negative"

def add_test(tests, positive, directory, testfile):
    if os.path.splitext(testfile)[1] != ".impala":
        return

    of = os.path.splitext(testfile)[0] + ".output"
    res = of if os.path.exists(of) else ""
    tests.append(InvokeTest(positive, directory, testfile, res))

def allTests():
    """
    This function returns a list of tests.
    """

    tests = []

    for f in os.listdir(POS_DIR):
        add_test(tests, True, POS_DIR, f)
    
    for f in os.listdir(NEG_DIR):
        add_test(tests, False, NEG_DIR, f)
    
    return tests

allTests()
