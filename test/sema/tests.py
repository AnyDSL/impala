"""
tests.py for semantic analysis tests
"""

# import the test infrastructure
from infrastructure.tests import InvokeTest, make_tests
import os

optionals = ["sema/positive/int_is_int32.impala",
    "sema/positive/subtyping1.impala"]

def allTests():
    """
    This function returns a list of tests.
    """
    tests = make_tests("sema/positive", True) + make_tests("sema/negative", False)
    
    # mark optionals
    for test in tests:
        if os.path.join(test.basedir, test.srcfile) in optionals:
            test.opt()
    
    return tests

