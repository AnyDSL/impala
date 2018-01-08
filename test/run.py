#!/usr/bin/env python

# get rid of '-p' --> done
# TODO search for impala binary in path
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

import os
import argparse
import sys
import subprocess
import filecmp

def argumentParser():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('path', nargs='*', help='path to test  or test directory', default='./', type=str)
    parser.add_argument('-b', '--binary', help='path to impala binary', default=None, type=str)
    parser.add_argument('-n', '--no-cleanup', action='store_true', default=False, dest='noCleanUp', help='keep log files after test run')
    parser.add_argument('-c', '--compile-timeout', help='timeout for compiling impala & clang',     default=5,             type=int)
    parser.add_argument('-r', '--run-timeout', help='timeout for running binary', default=10, type=int)
    args = parser.parse_args()
    return args

def binary(args):
    if args.binary != None:
        return args.binary
    p = subprocess.Popen(['printenv', 'PATH'],stdout=subprocess.PIPE)
    (out,err) = p.communicate()
    sout = str(out)[2:-3]
    list = sout.split(':')

    for dir in list:
        bin = dir + '/impala'
        if os.path.isfile(bin):
            return bin

    return '../build/bin/impala'

def giveCategorie(categories, file):
    with open(file) as rfile:
        line = rfile.readline()
        if line[:2]!='//':
            return 0 #default
        cat = line.strip('/').strip()
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
    return categories, tests 

def compareFiles(file1, file2): # True if equal, false otherwise
    if os.path.isfile(file1):
        if  not os.path.isfile(file2):
            return False
        return filecmp.cmp(file1, file2)
    else: 
        return False

def runCodegenTest(args, test): #0 passed 1 failed 2 timeout
    cmd = [args.binary]
    cmd.append(test[0])
    cmd.append('-emit-llvm')
    logname = test[1] +'.tmp.log'
    logfile = open(logname, 'w')
    try:
        p = subprocess.run(cmd, stderr=logfile, stdout=logfile, timeout=args.compile_timeout)
        if p.returncode!=0:
            print('failed here')
            return 1
        cmd = ['clang','-lm',test[1]+'.ll','lib.c','-o',test[1]]
        p = subprocess.run(cmd)
    except subprocess.TimeoutExpired as timeout:
        return 2   
    cmd2 = ['./'+test[1]]
    outputfile = test[1]+'.tmp.out'
    output = open(outputfile, 'w')
    try:
        p = subprocess.run(cmd2, stdout=output)
    except subprocess.TimeoutExpired as timeout:
        return 2 
    comparedOut = test[0][:-7]+'.out'
    diff =compareFiles(comparedOut, outputfile)

    if not args.noCleanUp:
        subprocess.run(['rm', test[1]+'.ll'])
        subprocess.run(['rm', outputfile])
        subprocess.run(['rm', logname])
        subprocess.run(['rm', test[1] ])

    if not diff:
        return 0
    return 1






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
            sys.stdout.write('[' + test[1] + '] : ' )
            testCounter+=1
            x = runCodegenTest(args, test)
            if x==0:
                successCounter+=1
                sys.stdout.write('passed\n')
                continue
            if x==2:
                timeoutCounter+=1
                sys.stdout.write('timed out\n')
            sys.stdout.write('failed\n')         
        categorieCounter+=1
        totalTestCounter+=testCounter
        totalSuccessCounter+=successCounter
        totalTimeoutCounter+=timeoutCounter
        sys.stdout.write('Tests: ' + str(testCounter) + ' Passed: ' + str(successCounter) + ' Timed out: ' + str(timeoutCounter) + ' Failed: ' + str(testCounter-successCounter-timeoutCounter) + '\n\n')
    sys.stdout.write('Total >>  Tests: ' + str(totalTestCounter) + ' Passed: ' + str(totalSuccessCounter) + ' Timed out: ' + str(totalTimeoutCounter) + ' Failed: ' + str(totalTestCounter-totalSuccessCounter-totalTimeoutCounter) + '\n\n')


log = open('log', 'w')
args =  argumentParser()
bin = binary(args)
args.binary = bin
executable = ['default', 'codegen']
print('set up testsuit')
categories, tests = setupTestSuit(args)
#sys.stdout.write('----------running Category ' + categories[0] + '----------\n')
runTests(categories, tests, log, args)


