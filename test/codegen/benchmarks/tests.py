"""
tests.py for codegen/benchmarks
"""

# import the test infrastructure
from infrastructure.tests import make_invoke_tests

optionals = [

]

args = {
    "codegen/benchmarks/aobench.impala" : [],
    "codegen/benchmarks/fannkuch.impala" : ["10"],
    "codegen/benchmarks/fasta.impala" : ["500000"],
    "codegen/benchmarks/mandelbrot.impala" : ["3000"],
    "codegen/benchmarks/meteor.impala" : ["2098"],
    "codegen/benchmarks/nbody.impala" : ["6000000"],
    "codegen/benchmarks/pidigits.impala" : ["10000"],
    "codegen/benchmarks/regex.impala" : [],
    "codegen/benchmarks/reverse.impala" : [],
    "codegen/benchmarks/spectral.impala" : ["1800"],
}

inputs = {
    "regex.impala" : "codegen/benchmarks/input_regex.txt",
    "reverse.impala" : "codegen/benchmarks/input_reverse.txt",
}

def allTests():
    """
    This function returns a list of tests.
    """
    tests = make_invoke_tests("codegen/benchmarks", [], True, {"aobench.impala" : "ao.ppm"}, inputs)
    
    # mark optionals
    for test in tests:
        if test.getName() in optionals:
            test.opt()

        if test.getName() in args:
            test.args = args[test.getName()]


    
    return tests
