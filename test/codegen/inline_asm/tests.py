"""
tests.py for codegen
"""

import platform

# import the test infrastructure
from infrastructure.tests import make_invoke_tests
from infrastructure.tests import get_tests_from_dir

def allTests():
    """
    This function returns a list of tests.
    """
    
    tests = make_invoke_tests("codegen/inline_asm")
    if platform.machine() == 'x86_64':
        tests += get_tests_from_dir("codegen/inline_asm/x86_64")

    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()
    
    return tests
