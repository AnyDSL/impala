"""
tests.py for codegen/inline_asm
"""

# import the test infrastructure
from infrastructure.tests import make_invoke_tests

optionals = [

]

args = {
    "codegen/inline_asm/asm_simple_1.impala" : [],
    "codegen/inline_asm/asm_simple_2.impala" : [],
    "codegen/inline_asm/asm_atomic_decrement.impala" : [],
}

def allTests():
    """
    This function returns a list of tests.
    """
    tests = make_invoke_tests("codegen/inline_asm")
    
    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()

        if test.getName() in args:
            test.args = args[test.getName()]


    
    return tests
