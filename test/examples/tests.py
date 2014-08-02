"""
This folder contains full impala examples that are ready to run.
tests.py runs code generation tests for those examples (TODO currently codegen is disabled)
"""

# import the test infrastructure
from infrastructure.tests import make_tests
import os

optionals = []

def allTests():
    """This functions returns all the tests in this directory"""
    tests = make_tests("examples", True)
    
    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()
    
    return tests
