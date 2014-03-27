'''
Created on 8 Dec 2013

@author: Alexander Kampmann, David Poetzsch-Heffter
'''

import sys, os, subprocess, difflib, shutil
from pb import Progressbar

class Test:
    """Superclass for all the tests."""
    optional = False
    
    def opt(self):
        self.optional = True
        return self
        
    def isOptional(self):
        return self.optional
    
class InvokeTest(Test):
    """Superclass tests which work on a single file and compare the output."""
    positive = True
    basedir = "."
    srcfile = ""
    options = ""
    result = ""
    
    def __init__(self, positive, base, src, res, options=[]):
        self.positive = positive
        self.basedir = base
        self.srcfile = src
        self.result = res
        self.options = options
    
    def invoke(self, gEx):
        #print("Start test "+str(self.getName()))
        execCmd = [os.path.abspath(gEx)] + self.options + [self.srcfile]
        try:
            output = subprocess.check_output(execCmd, cwd=self.basedir, stderr=subprocess.STDOUT)
            return self.checkOutput(True, output);
        except subprocess.CalledProcessError as err:
            return self.checkOutput(False, err.output);
            
    def checkOutput(self, success, output):
        if success != self.positive:
            print("\n[FAIL] "+os.path.join(self.basedir, self.srcfile))
            print("Output: "+output)
            return False
        if "" == self.result:
            return True
    
        with open(os.path.join(self.basedir, self.result), 'r') as f:
            output = output.splitlines(1)
            expected = f.read().splitlines(1)
            diff = difflib.Differ()
            fails = 0
            for cp in diff.compare(expected, output):
                if cp.startswith('-') or cp.startswith('+'):
                    print(cp.rstrip())
                    fails=fails+1
            
            return True if fails == 0 else False
        
    def getName(self):
        return os.path.join(self.basedir, self.srcfile)

def make_tests(directory, positive=True, options=[]):
    """Create a list of tests based on the .impala files in directory
    
    Output files are expected to have the same name but with .output extension.
    If no output file is found for a test no output is assumed."""
    tests = []

    for testfile in os.listdir(directory):
        if os.path.splitext(testfile)[1] == ".impala":
            of = os.path.splitext(testfile)[0] + ".output"
            res = of if os.path.exists(os.path.join(directory, of)) else ""
            tests.append(InvokeTest(positive, directory, testfile, res, options))
    
    return sorted(tests, key=lambda test: test.getName())

def executeTests(tests, gEx, pb = True):
    """Invoke this function with a list of test objects to run the tests. """
    
    res = {}
    bar = Progressbar(50)
    s   = True
    for i in range(len(tests)):
        if pb:
            bar.update(float(i)/float(len(tests)),tests[i].getName(),success=s)
        else:
            print ("["+str(i+1)+"/"+str(len(tests))+"] " + tests[i].getName())
        res[tests[i]] = s = tests[i].invoke(gEx)
    bar.done()

    print("\n* Test summary")
    failOpt = 0
    failReq = 0
    
    opt_tests = []
    req_tests = []
    for t in tests:
        opt_tests.append(t) if t.isOptional() else req_tests.append(t)
    
    for t in req_tests:
        if not res[t]:
            print("\n - REQUIRED test failed: "+t.getName())
            failReq += 1
            
    for t in opt_tests:
        if not res[t]:
            print("\n - OPTIONAL test failed: "+t.getName())
            failOpt += 1
    
    if failOpt == 0 and failReq == 0:
        print("\n* All " + str(len(tests)) +  " tests were successful.")
    else:
        if failReq == 0:
            print("\n* All required tests were successful.")
        else:
            print("\n!" + str(failReq) + " of " + str(len(req_tests)) + " REQUIRED tests failed.")
        if failOpt == 0:
            print("\n* All optional tests were successful.")
        else:
            print("\n!" + str(failOpt) + " of " + str(len(opt_tests)) + " OPTIONAL tests failed.")
    
    
def concat(outDir, srcFiles):
    def doConcat():
        if not os.path.exists(outDir):
            os.makedirs(outDir)
        try:
            res = []
            for srcs in srcFiles:
                lastSrc = srcs[-1]
                out = lastSrc[len(os.path.dirname(lastSrc))+1:]
                out = os.path.abspath(os.path.join(outDir, out))
                with open(out, 'w+') as dest:
                    for s in srcs:
                        shutil.copyfileobj(open(s, 'rb'), dest)
                res.append(out)
            return res
        except subprocess.CalledProcessError as err:
            print("Preprocessor failure: ")
            print(err.output)
            return []
    return doConcat
