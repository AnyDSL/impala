"""
tests.py for codegen/inline_asm
"""

# import the test infrastructure
from infrastructure.tests import make_invoke_tests

optionals = [

]

args = {
    "codegen/inline_asm/x86_64/asm_syscall.impala" : [],
}

def allTests():
    """
    This function returns a list of tests.
    """
    tests = make_invoke_tests("codegen/inline_asm/x86_64")
    
    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()

        if test.getName() in args:
            test.args = args[test.getName()]


    
    return tests
