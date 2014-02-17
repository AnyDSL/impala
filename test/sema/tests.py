"""
tests.py for semantic analysis tests
"""

import os

# import the test infrastructure
from infrastructure.tests import PositiveTest, NegativeTest, ProgramTest, concat

POS_DIR = "sema/positive"
NEG_DIR = "sema/positive"

def allTests():
    """
    This function returns a list of tests.
    """

    tests = []

    for f in os.listdir(POS_DIR):
        of = os.path.splitext(f)[0] + ".output"
        res = os.path.exists(of) ? of : ""
        tests.append(PositiveTest(POS_DIR, f, res))
    
    # The parameters are: baseDir, input file name, expected output, compiler options
    # expected output is a file name for positive tests and an error string otherwise
    # calling opt() indicates that a test tests optional features
    
    return tests

allTests()
