"""
tests.py for semantic analysis tests
"""

# import the test infrastructure
from infrastructure.tests import InvokeTest, make_tests
import os

optionals = []

def allTests():
    """
    This function returns a list of tests.
    """
    tests = make_tests("type_inference/positive", True) + make_tests("type_inference/negative", False)
    
    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()
    
    return tests

