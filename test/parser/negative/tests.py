"""
tests.py for parser tests
"""

# import the test infrastructure
from infrastructure.tests import make_tests
import os

def allTests():
    """
    This function returns a list of tests for the parser.
    """
    
    return make_tests("parser/negative", False)

