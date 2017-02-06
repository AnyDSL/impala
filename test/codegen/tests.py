"""
tests.py for codegen
"""

# import the test infrastructure
from infrastructure.tests import make_invoke_tests
from infrastructure.tests import get_tests_from_dir

def allTests():
    """
    This function returns a list of tests.
    """

    optionals = [
      "codegen/bind_ret_inner_fn.impala",
      "codegen/conversion_trait.impala",
      "codegen/diderot.impala",
      "codegen/endless_mangling.impala",
      "codegen/fold.impala",
      "codegen/generic_get.impala",
      "codegen/generic_while.impala",
      "codegen/ldg.impala",
      "codegen/mem2reg_bug2.impala",
      "codegen/parallel.impala",
      "codegen/poly_id.impala",
      "codegen/poly_sort.impala",
      "codegen/poly_sq.impala",
      "codegen/poly_type_arg.impala",
      "codegen/primes.impala",
      "codegen/range.impala",
      "codegen/range_poly.impala",
      "codegen/ret_assert.impala",
      "codegen/spir_phi_bug.impala",
      "codegen/struct_arg.impala",
      "codegen/system_f_problem.impala",
      "codegen/trait_impls.impala",
      "codegen/while_true.impala",
    ]

    tests = make_invoke_tests("codegen")
    tests += get_tests_from_dir("codegen/benchmarks")
    tests += get_tests_from_dir("codegen/inline_asm")

    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()

    return tests
