'''
Created on 8 Dec 2013

@author: Alexander Kampmann, David Poetzsch-Heffter
'''

import sys, os, difflib, shutil, imp, tempfile
from timed_process import CompileProcess, RuntimeProcess
from valgrindxml import ValgrindXML

class Test(object):
    """Superclass for all the tests."""
    optional = False
    
    def __init__(self, base, src, options, optional=False):
        self.basedir = base
        self.srcfile = src
        self.options = options
        self.optional = optional
    
    def opt(self):
        self.optional = True
        return self
        
    def isOptional(self):
        return self.optional
    
    def getName(self):
        return os.path.join(self.basedir, self.srcfile)
    
    def checkBasics(self, proc):
        cmd = os.path.basename(proc.cmd[0])
        
        if proc.killed:
            print("[FAIL] " + os.path.join(self.basedir, self.srcfile))
            print("  Process '%s' timed out." % cmd)
            print
            return False
        
        if proc.crash():
            print("[FAIL] " + os.path.join(self.basedir, self.srcfile))
            print("  '%s' crashed. Return code was: %d" % (cmd, proc.returncode))
            print
            return False
        
        return True
    
class ValgrindTest(Test):
    VALGRIND_XML_FILE = os.path.join(tempfile.gettempdir(), "impala_valgrind.xml")
    VALGRIND_TIMEOUT_FACTOR = 5
    
    def __init__(self, test):
        super(ValgrindTest, self).__init__(test.basedir, test.srcfile, test.options, test.isOptional())
    
    def invoke(self, gEx):
        execCmd = ["valgrind", "--xml=yes", "--xml-file="+ValgrindTest.VALGRIND_XML_FILE]
        execCmd += [gEx] + self.options + [self.srcfile]
        
        timeout = CompileProcess.timeout
        if timeout == CompileProcess.DEFAULT_TIMEOUT:
            timeout *= ValgrindTest.VALGRIND_TIMEOUT_FACTOR
        
        p = CompileProcess(execCmd, self.basedir, timeout)
        p.execute()
        return self.checkBasics(p) and self.checkValgrind(p)
    
    def checkValgrind(self, p):
        try:
            vgout = ValgrindXML(ValgrindTest.VALGRIND_XML_FILE)

            success = len(vgout.leaks) == 0

            if not success:
                print("[FAIL] " + os.path.join(self.basedir, self.srcfile))
                print
                print(vgout)
            
            return success
        except Exception as e:
            print("Parsing valgrind output FAILED: %s" % e)
            return False

def diff_output(output, expected):
    olines = output.splitlines(1)
    elines = expected.splitlines(1)
    
    diff = difflib.Differ()
    fails = 0
    for cp in diff.compare(elines, olines):
        if cp.startswith('-') or cp.startswith('+'):
            print(cp.rstrip())
            fails=fails+1
    
    return True if fails == 0 else False

class CompilerOutputTest(Test):
    """Superclass tests which work on a single file and compare the output."""
    positive = True
    basedir = "."
    srcfile = ""
    options = ""
    result = None
    
    def __init__(self, positive, base, src, res, options=[]):
        super(CompilerOutputTest, self).__init__(base, src, options)
        self.positive = positive
        self.result = res
    
    def invoke(self, gEx):
        execCmd = [gEx] + self.options + [self.srcfile]
        p = CompileProcess(execCmd, self.basedir)
        p.execute()
        return self.checkBasics(p) and self.checkOutput(p)

    def checkOutput(self, p):
        if p.success() != self.positive:
            print("[FAIL] "+os.path.join(self.basedir, self.srcfile))
            print("Output: "+p.output)
            print
            return False
        
        if self.result is None:
            return True
    
        with open(os.path.join(self.basedir, self.result), 'r') as f:
            return diff_output(p.output, f.read())

class InvokeTest(Test):
    """Superclass tests which work on a single file and compare the output."""
    positive = True
    basedir = "."
    srcfile = ""
    options = ""
    output_file = None
    compare_file = None
    input_file = None

    args = None

    LIB_C = os.path.join(os.path.dirname(__file__), "lib.c")
    CALL_IMPALA_MAIN_C = os.path.join(os.path.dirname(__file__), "call_impala_main.c")
    CLANG = os.path.join(os.path.dirname(__file__), "..", "..", "..", "llvm_install", "bin", "clang")
    
    def __init__(self, base, src, output_file, options=[], benchmarks=False, compare=None, input_file=None):
        super(InvokeTest, self).__init__(base, src, options+["-emit-llvm"])
        self.output_file = output_file
        
        basename = os.path.splitext(self.srcfile)[0]
        self.ll_file = basename + ".ll"
        self.bc_file = basename + ".bc"
        self.s_file = basename + ".s"
        self.exe_file = basename
        self.tmp_files = [self.ll_file, self.bc_file, self.s_file, self.exe_file]
        self.benchmarks = benchmarks

        if compare is not None:
            self.compare_file = compare
            self.tmp_files.append(self.compare_file)

        self.input_file = input_file
    
    def compilePhases(self, gEx):
        yield [gEx] + self.options + [os.path.join(self.basedir, self.srcfile)]
        if(self.benchmarks):
            yield [InvokeTest.CLANG, "-O3", InvokeTest.LIB_C, "-c"]
            yield [InvokeTest.CLANG, "-O3", "lib.o", self.ll_file, "-L", "/opt/local/lib", "-lm", "-lpcre", "-lgmp", "-s", "-o", self.exe_file]
        else:
            yield ["llc", "-o", self.s_file, self.bc_file]
            yield ["cc", "-o", self.exe_file, self.s_file, InvokeTest.CALL_IMPALA_MAIN_C]

    
    def invoke(self, gEx):
        # if any tmp file already exists do not touch it and fail
        #for tmp in self.tmp_files:
        #    if os.path.exists(tmp):
        #        print("[FAIL] "+os.path.join(self.basedir, self.srcfile))
        #        print("  Will not overwrite existing file '%s'; please clean up before running tests" % tmp)
        #        print
        #        return False

        try:
            for phase in self.compilePhases(gEx):
                p = CompileProcess(phase, ".")
                p.execute()
                if not (self.checkBasics(p) and self.compilationSuccess(p)):
                    return False
            
            # run executable
            if self.args is None:
                p = RuntimeProcess([os.path.join(".", self.exe_file)], ".")
            else:
                cmd = [os.path.join(".", self.exe_file)]
                cmd.extend(self.args)
                p = RuntimeProcess(cmd, ".")

            if self.input_file is not None:
                p.setInput(self.input_file)

            p.execute()
            return self.checkBasics(p) and self.checkOutput(p)
        finally:
            # cleanup
            for tmp in self.tmp_files:
                self.cleanup(tmp)
                pass
    
    def checkOutput(self, proc):
        if not proc.success():
            print("[FAIL] "+os.path.join(self.basedir, self.srcfile))
            print("  Return code was '%d'" % proc.returncode)
            print
            return False
        
        if self.output_file is not None:
            if self.compare_file is None:
                #Image.open();
                with open(os.path.join(self.basedir, self.output_file), 'r') as f:
                    return diff_output(proc.output, f.read())
            else:
                with open(self.compare_file, 'r') as f:
                    with open(os.path.join(self.basedir, self.output_file), 'r') as g:
                        return diff_output(f.read(), g.read())
        return True
            
    def cleanup(self, file):
        if os.path.exists(file):
            os.remove(file)

    def compilationSuccess(self, p):
        if not p.success():
            print("\n[FAIL] "+os.path.join(self.basedir, self.srcfile))
            print("Output: "+p.output)
            return False
        return True

def get_tests(directory):
    """A generator for test files based on the .impala files in directory
    
    Output files are expected to have the same name but with .output extension.
    If no output file is found for a test no output is assumed.
    
    This yields (test_file, output_file) for each .impala file in the directory"""
    tests = []

    for testfile in os.listdir(directory):
        if os.path.splitext(testfile)[1] == ".impala":
            of = os.path.splitext(testfile)[0] + ".output"
            res = of if os.path.exists(os.path.join(directory, of)) else None
            yield (testfile, res)

def make_compiler_output_tests(directory, positive=True, options=[]):
    """Creates a list of CompilerOutputTests using get_tests(directory)"""
    tests = []
    for testfile, res in get_tests(directory):
        tests.append(CompilerOutputTest(positive, directory, testfile, res, options))
    return sorted(tests, key=lambda test: test.getName())

def make_tests(directory, positive=True, options=[]):
    return make_compiler_output_tests(directory, positive, options)

def make_invoke_tests(directory, options=[], benchmarks=False, testToFile={}, inputs={}):
    """Creates a list of InvokeTests using get_tests(directory)"""
    tests = []
    for testfile, res in get_tests(directory):
        input_file = inputs[testfile] if testfile in inputs else None
        #print input_file, testfile
        if testfile in testToFile:
            tests.append(InvokeTest(directory, testfile, res, options, benchmarks, testToFile[testfile], input_file))
        else:
            tests.append(InvokeTest(directory, testfile, res, options, benchmarks, None, input_file))
    return sorted(tests, key=lambda test: test.getName())

def get_tests_for_file(file):
    directory = os.path.dirname(file)
    filename = os.path.basename(file)
    testfile = os.path.join(directory, "tests.py")

    if os.path.exists(testfile):
        tests = imp.load_source("tests", testfile).allTests()
    else:
        tests = make_tests(directory)

    for test in tests:
        if test.srcfile == filename:
            return [test]

    sys.exit("Test not found!")

def get_tests_from_dir(directory):
    testfile = os.path.join(directory, "tests.py")
    
    if os.path.exists(testfile):
        tests = imp.load_source("tests", testfile).allTests()
    else:
        tests = make_tests(directory)
    return tests

def executeTests(tests, gEx):
    """Invoke this function with a list of test objects to run the tests. """
    
    res = {}
    s   = True
    for i in range(len(tests)):
        print ("["+str(i+1)+"/"+str(len(tests))+"] " + tests[i].getName())
        res[tests[i]] = s = tests[i].invoke(gEx)

    print("\n* Test summary\n")
    failOpt = 0
    failReq = 0
    passOpt = []
    
    opt_tests = []
    req_tests = []
    for t in tests:
        opt_tests.append(t) if t.isOptional() else req_tests.append(t)
    
    for t in req_tests:
        if not res[t]:
            print("- REQUIRED test failed: "+t.getName())
            failReq += 1
            
    for t in opt_tests:
        if not res[t]:
            print("- OPTIONAL test failed: "+t.getName())
            failOpt += 1
        else:
            passOpt.append(t.getName())

    for test in passOpt:
        print("- OPTIONAL test passed: "+test)
    
    if failOpt == 0 and failReq == 0:
        print("\n* All " + str(len(tests)) +  " tests were successful.")
    else:
        if failReq == 0:
            print("\n* All %i required tests were successful." % len(req_tests))
        else:
            print("\n!" + str(failReq) + " of " + str(len(req_tests)) + " REQUIRED tests failed.")
        if failOpt == 0:
            print("\n* All %i optional tests were successful." % len(opt_tests))
        else:
            print("\n!" + str(failOpt) + " of " + str(len(opt_tests)) + " OPTIONAL tests failed.")
            
    return -1 if failReq > 0 else 0
