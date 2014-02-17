"""
tests.py for semantic analysis tests
"""

# import the test infrastructure
from infrastructure.tests import InvokeTest, make_tests

def allTests():
    """
    This function returns a list of tests.
    """
    return make_tests("sema/positive", True) + make_tests("sema/negative", False)

