"""
tests.py for codegen
"""

# import the test infrastructure
from infrastructure.tests import make_tests
import os

def allTests():
    """
    This function returns a list of tests.
    """
    tests = make_tests("codegen", True, ["--emit-llvm"])
    
    return tests
