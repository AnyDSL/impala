#!/usr/bin/env python

# TODO search for clang binary in path
# priority:
# - option --> done
# - $PATH--> done
# - directory structure --> done
#
# codegen tests:
# -emit-llvm
# clang -lm -l... 
#
#  codegen/foo.impala
#  codegen/foo.log
#  codegen/foo.in
#  codegen/foo.out
#  -> foo.tmp.log
#  -> foo.ll
#  -> foo
#  -> foo.tmp.out
#  diff foo.out codegen/foo.output  (see infrastructure/tests.py)
# 
#  - deal with foo.in for stdin
#  - deal with 'broken'
#  - deal with cmd line arguments for the test case (see codegen/benchmarks/fannkuch.impala)
#  - deal with linker option e.g. '-lm' (in first-line comment)
#  - sort test cases

import os
import argparse
import sys
import subprocess
import filecmp

SUCCESS = 0
FAILED = 1
TIMEDOUT = 2

def argumentParser():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('path', nargs='*',          help='path to test  or test directory',      default='./', type=str)
    parser.add_argument('-c',  '--clang',           help='path to clang binary',                 default=None, type=str) # TODO
    parser.add_argument('-i',  '--impala',          help='path to impala binary',                default=None, type=str)
    parser.add_argument('-ct', '--compile-timeout', help='timeout for compiling impala & clang', default=5,    type=int)
    parser.add_argument('-rt', '--run-timeout',     help='timeout for running binary',           default=10,   type=int)
    parser.add_argument('-b',  '--broken',          help='also run broken tests',                default=False, action='store_true', dest='broken')
    parser.add_argument('-n',  '--no-cleanup',      help='keep log files after test run',        default=False, action='store_true', dest='noCleanUp')
    args = parser.parse_args()
    return args

def find_impala(args):
    if args.impala != None:
        return args.impala
    p = subprocess.Popen(['printenv', 'PATH'],stdout=subprocess.PIPE)
    (out,err) = p.communicate()
    sout = str(out)[2:-3]
    list = sout.split(':')

    for dir in list:
        bin = dir + '/impala'
        if os.path.isfile(bin):
            return bin

    return '../build/bin/impala'

def find_clang(args):
    if args.clang != None:
        return args.clang
    p = subprocess.Popen(['printenv', 'PATH'],stdout=subprocess.PIPE)
    (out,err) = p.communicate()
    sout = str(out)[2:-3]
    list = sout.split(':')

    for dir in list:
        bin = dir + '/clang'
        if os.path.isfile(bin):
            return bin

    return 'clang'

def readFirstLine(file):
    with open(file) as rfile:
        line = rfile.readline()
        if line[:2]!='//':
            return None
        return line[2:].split()

def isBroken(X):
    for x in X:
        if (x=='broken'):
            res = []
            for y in X:
                if y!=x:
                    res.append(y)
            return True,res
    return False, X

def giveCategorie(categories, file):
    with open(file) as rfile:
        line = rfile.readline()
        if line[:2]!='//':
            return 0 #default
        cat = line.strip('/').strip()
        # TODO use 'if' here and report warning in stderr if applicable
        try:
            num = categories[cat]
            return num
        except:
            return 0

def sortIn(categories, tests, file):
    cat = giveCategorie(categories, file)
    testpath = file.split('/')
    testname = testpath[-1][:-7]
    entry = [file,testname]
    return cat,entry

def setupTestSuit(args):
    categories = {}
    categories['undefined']=0
    categories['codegen']=1
    categories['sema']=2
    categories['type_inferr']=3
    tests = [[],[],[],[]]    
    
    if args.path==[]:
        args.path.append('./')

    for x in args.path:
        if os.path.isfile(x):
            (cat,entry) = sortIn(categories,tests,x)
            tests[cat].append(entry)
            continue

        for subdir, dirs, files in os.walk(x):
            for file in files:
                if (file[-7:]=='.impala'):
                    cat, entry = sortIn(categories,tests,os.path.join(subdir, file))
                    tests[cat].append(entry)
    sortedTests = []  
    for t in tests:
        sortedTests.append(sorted(t))             
    return categories, sortedTests 

def compareFiles(tmp_out, out): # True if equal, false otherwise
    if os.path.isfile(out):
        return filecmp.cmp(tmp_out, out)
    else: 
        return True

# TODO use constants instead of magic numbers as return value
def runCodegenTest(args, test, arguments): #0 passed 1 failed 2 timeout
    cmd = [args.impala]
    cmd.append(test[0])
    cmd.append('-emit-llvm')
    cmd.extend(arguments)
    logname = test[1] +'.tmp.log'
    logfile = open(logname, 'w')
    try:
        p = subprocess.run(cmd, stderr=logfile, stdout=logfile, timeout=args.compile_timeout)
        if p.returncode!=0:
            print('failed here')
            return FAILED
        cmd = [args.clang,'-lm',test[1]+'.ll','lib.c','-o',test[1]]
        p = subprocess.run(cmd)
    except subprocess.TimeoutExpired as timeout:
        return TIMEDOUT  
    cmd2 = ['./'+test[1]]
    inputFile = test[0][:-7] + '.in'
    if os.path.isfile(inputFile):
        cmd2.append(inputFile)
    tmp_out = test[1]+'.tmp.out'
    output = open(tmp_out, 'w')
    try:
        p = subprocess.run(cmd2, stdout=output, timeout=args.run_timeout)
    except subprocess.TimeoutExpired as timeout:
        return TIMEDOUT 
    out = test[0][:-7]+'.out'
    diff = compareFiles(tmp_out, out)

    if not args.noCleanUp:
        subprocess.run(['rm', test[1]+'.ll'])
        subprocess.run(['rm', tmp_out])
        subprocess.run(['rm', logname])
        subprocess.run(['rm', test[1] ])

    if diff:
        return SUCCESS
    return FAILED

def runTests(categories, tests, log, args):
    categorieCounter = 0
    totalTestCounter = 0
    totalSuccessCounter = 0
    totalTimeoutCounter = 0
    executable = ['codegen']
    for exec in executable:
        index = categories[exec]
        testsuit = tests[index]
        sys.stdout.write('----------running Category ' + exec + '----------\n')
        testCounter=0
        successCounter=0
        timeoutCounter=0
        for test in testsuit:
            firstLine = readFirstLine(test[0])
            broken, arguments = isBroken(firstLine)
            if  (not args.broken) and broken:
                continue
            sys.stdout.write('[' + test[0] + '] : ' )
            testCounter+=1
            x = runCodegenTest(args, test, arguments[1:])
            if x==SUCCESS:
                successCounter+=1
                sys.stdout.write('passed\n')
                continue
            if x==TIMEDOUT:
                timeoutCounter+=1
                sys.stdout.write('timed out\n')
                continue
            sys.stdout.write('failed\n')         
        categorieCounter+=1
        totalTestCounter+=testCounter
        totalSuccessCounter+=successCounter
        totalTimeoutCounter+=timeoutCounter
        sys.stdout.write('Tests: ' + str(testCounter) + ' Passed: ' + str(successCounter) + ' Timed out: ' + str(timeoutCounter) + ' Failed: ' + str(testCounter-successCounter-timeoutCounter) + '\n\n')
    sys.stdout.write('Total >>  Tests: ' + str(totalTestCounter) + ' Passed: ' + str(totalSuccessCounter) + ' Timed out: ' + str(totalTimeoutCounter) + ' Failed: ' + str(totalTestCounter-totalSuccessCounter-totalTimeoutCounter) + '\n\n')


log = open('log', 'w')
args =  argumentParser()

impala = find_impala(args)
args.impala = impala

clang = find_clang(args)
args.clang = clang


print('set up testsuit')
categories, tests = setupTestSuit(args)
print(tests)
runTests(categories, tests, log, args)
