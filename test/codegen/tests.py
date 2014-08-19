"""
tests.py for codegen
"""

# import the test infrastructure
from infrastructure.tests import make_invoke_tests

optionals = []

def allTests():
    """
    This function returns a list of tests.
    """
    tests = make_invoke_tests("codegen")
    
    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()
    
    return tests
