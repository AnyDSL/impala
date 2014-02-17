'''
Created on 8 Dec 2013

@author: Alexander Kampmann
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
                    print(cp)
                    fails=fails+1
            if 0!=fails:
                return False
        
    def getName(self):
        return os.path.join(self.basedir, self.srcfile)
    
def executeTests(tests, gEx, pb = True):
    """Invoke this function with a list of test objects to run the tests. """
    
    res = []
    bar = Progressbar(50)
    s   = True
    for i in range(len(tests)):
        if pb:
            bar.update(float(i)/float(len(tests)),tests[i].getName(),success=s)
        else:
            print ("["+str(i+1)+"/"+str(len(tests))+"] " + tests[i].getName())
        opt = tests[i]
        s   = opt.invoke(gEx)
        res.append(s)
    bar.done()

    print ("\n* Test summary")    
    failOpt = 0
    allOpt = 0
    failReq = 0
    allReq = 0
    for i in range(0, len(tests)):
        if not res[i]:
            if tests[i].isOptional():
                print("\n - optional test failed: "+tests[i].getName())
                failOpt = failOpt + 1
            else:
                print("\n - required test failed: "+tests[i].getName())
                failReq = failReq + 1
        if tests[i].isOptional():
            allOpt = allOpt + 1
        else:
            allReq = allReq + 1
    if failOpt == 0 and failReq == 0:
        print("\n* All " + str(len(tests)) +  " tests were successful.")
    else:
        if failReq == 0:
            print("\n* All required tests were successful.")
        else:
            print("\n!" + str(failReq) + " of " + str(allReq) + " REQUIRED tests failed.")
        if failOpt == 0:
            print("\n* All optional tests were successful.")
        else:
            print("\n!" + str(failOpt) + " of " + str(allOpt) + " OPTIONAL tests failed.")

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

class ProgramTest(Test):
    def __init__(self, name, outDir, srcData, inputs, optimize=False):
        self.name = name
        self.outDir = outDir
        self.srcData = srcData
        self.inputs = inputs
        self.optimize = optimize
    
    def invoke(self, gEx):
        if not os.path.exists(os.path.join(self.outDir, "c4")):
            os.makedirs(os.path.join(self.outDir, "c4"))
        if not os.path.exists(os.path.join(self.outDir, "clang")):
            os.makedirs(os.path.join(self.outDir, "clang"))

        if hasattr(self.srcData, '__call__'):
            self.srcFiles = self.srcData()
        else:
            self.srcFiles = self.srcData
        if not self.compile(gEx):
            self.clean(self.outDir)
            return False
        if not self.link():
            self.clean(self.outDir)
            return False
        result = self.run()
        self.clean(self.outDir)
        #print(self.name+": "+str(result))
        return result

    def compile(self,gEx):
        result = True
        # compile all src files
        for src in self.srcFiles:
            try:
                cmd = [os.path.abspath(gEx)]
                if(self.optimize):
                    cmd.append("--optimize")
                cmd.append(os.path.abspath(src))
                subprocess.check_output(cmd, cwd=os.path.join(self.outDir, "c4"), stderr=subprocess.STDOUT)
            except subprocess.CalledProcessError as err:
                print("c4 error: "+src+":")
                print(err.output)
                result = False
            try:
                cmd = ["clang", "-emit-llvm", "-c", "-S", "-o", self.unitName(src), os.path.abspath(src)]
                subprocess.check_output(cmd, cwd=os.path.join(self.outDir, "clang"), stderr=subprocess.STDOUT)
            except subprocess.CalledProcessError as err:
                print("clang error: "+src+":")
                print(err.output)
                result = False
            except:
                result = False
        return result
    
    def unitName(self, src):
        return src[len(os.path.dirname(src))+1:-2]+".ll"
        
    def link(self):
        # link all src files
        units = []
        for src in self.srcFiles:
            unit = self.unitName(src)
            units.append(unit)
        try:
            cmd = ["llvm-link", "-o=output.ll"]+units
            subprocess.check_output(cmd, cwd=os.path.join(self.outDir, "c4"), stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError as err:
            print("Could not link c4's output: "+src+":")
            print(err.output)
            return False
        
        try:
            cmd = ["llvm-link", "-o=output.ll"]+units
            subprocess.check_output(cmd, cwd=os.path.join(self.outDir, "clang"), stderr=subprocess.STDOUT)
        except subprocess.CalledProcessError as err:
            print("Could not link clang's output: "+str(units)+":")
            print(err.output)
            return False
        return True
    
    def run(self):
        cmdC4 = ["lli", os.path.join("c4", "output.ll")]
        cmdC4 = cmdC4 + self.inputs
        cmdClang = ["lli", os.path.join("clang", "output.ll")]
        cmdClang = cmdClang + self.inputs
        try:
            outputC4 = subprocess.check_output(cmdC4, cwd=self.outDir, stderr=subprocess.STDOUT)
            outputClang = subprocess.check_output(cmdClang, cwd=self.outDir, stderr=subprocess.STDOUT)
            return self.compare(outputC4, outputClang)
        except subprocess.CalledProcessError as err:
            print("Could not execute binary:")
            print(err.output)
            return False
    
    def compare(self, c4, clang):
        c4 = c4.splitlines(1)
        clang = clang.splitlines(1)
        diff = difflib.Differ()
        fails = 0
        for cp in diff.compare(clang, c4):
            if cp.startswith('-') or cp.startswith('+'):
                print(cp)
                fails=fails+1
        if 0!=fails:
            return False
        return True
    
    def clean(self, dire):
        return
        
        for f in os.listdir(dire):
            f = os.path.join(dire, f)
            if os.path.isdir(f):
                self.clean(f)
                os.rmdir(f)
            else:
                os.remove(f)
    
    def getName(self):
        return self.name
