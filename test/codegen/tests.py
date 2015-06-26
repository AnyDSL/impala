"""
tests.py for codegen
"""

# import the test infrastructure
from infrastructure.tests import make_invoke_tests
from infrastructure.tests import get_tests_from_dir

optionals = [
    "codegen/conversion_trait.impala",
    "codegen/diderot.impala",
    "codegen/endless_mangling.impala",
    "codegen/generic_get.impala",
    "codegen/generic_while.impala",
    "codegen/primes.impala",
    "codegen/range.impala",
    "codegen/range_poly.impala",
    "codegen/ret_assert.impala",
    "codegen/steensgard.impala",
    "codegen/system_f_problem.impala",
    "codegen/trait_impls.impala",
    "codegen/vcycle.impala",
    "codegen/zip.impala"
]

def allTests():
    """
    This function returns a list of tests.
    """
    tests = make_invoke_tests("codegen")
    tests += get_tests_from_dir("codegen/benchmarks")

    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()
    
    return tests
