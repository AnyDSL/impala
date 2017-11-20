"""
tests.py for parser tests
"""

# import the test infrastructure
from infrastructure.tests import get_tests_from_dir
import os

def allTests():
    """
    This function returns a list of tests for the parser.
    """

    tests = get_tests_from_dir("parser/negative") + get_tests_from_dir("parser/positive")
    
    return tests

