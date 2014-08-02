"""
tests.py that runs code generation tests and executes the programs to check against expected output  (TODO currently code generation is disabled)
"""

# import the test infrastructure
from infrastructure.tests import make_tests
import os

optionals = []

def allTests():
    """This functions returns all the tests in this directory"""
    tests = make_tests("codegen", True)
    
    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()
    
    return tests
