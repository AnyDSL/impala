"""
tests.py for semantic analysis tests
"""

# import the test infrastructure
from infrastructure.tests import make_tests
import os

optionals = [
    "sema/negative/incorrect_param_types.impala",
    "sema/negative/missing_typevar.impala",
    "sema/negative/mutual_trait_refs.impala",
    "sema/negative/scoping1.impala",
    "sema/negative/scoping2.impala",
    "sema/negative/static.impala",
    "sema/negative/super_traits1.impala",
    "sema/negative/super_traits2.impala",
    "sema/negative/super_traits3.impala",
    "sema/negative/trait_impls2.impala",
    "sema/negative/trait_impls4.impala",
    "sema/negative/trait_impls5.impala",
]

def allTests():
    """
    This function returns a list of tests.
    """
    tests = make_tests("sema/negative", False)
    
    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()
    
    return tests

