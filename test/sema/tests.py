"""
tests.py for semantic analysis tests
"""

# import the test infrastructure
from infrastructure.tests import make_tests
import os

optionals = ["sema/positive/int_is_int32.impala",
    "sema/positive/subtyping1.impala",
    "sema/positive/subtyping5.impala",
    "sema/positive/map.impala",
    "sema/positive/methods7.impala",
    "sema/positive/methods8.impala"
]

def allTests():
    """
    This function returns a list of tests.
    """
    tests = make_tests("sema/positive", True) + make_tests("sema/negative", False)
    
    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()
    
    return tests

