"""
tests.py that globally runs all tests
"""

# import the test infrastructure
from infrastructure.tests import make_tests, get_tests_from_dir
import os

optionals = []

def getTestsOfThisDir():
    """This functions returns all the tests in this directory"""
    tests = make_tests(".", True)
    
    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()
    
    return tests


def allTests():
    """This function returns a list of tests."""

    tests = []

    # TODO include tests from this directory
    # tests += getTestsOfThisDir()
    
    for d in os.listdir("."):
        if os.path.isdir(d) and d != "infrastructure":
            tests += get_tests_from_dir(d)
    
    return tests
