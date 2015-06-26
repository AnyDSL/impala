"""
tests.py for semantic analysis tests
"""

# import the test infrastructure
from infrastructure.tests import get_tests_from_dir
import os

def allTests():
    """
    This function returns a list of tests.
    """
    tests = get_tests_from_dir("type_inference/positive") + get_tests_from_dir("type_inference/negative")
    
    # mark optionals
    #for test in tests:
    #    if test.getName() in optionals:
    #        test.opt()
    
    return tests

