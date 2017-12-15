import os
import argparse
import sys
import subprocess


def argumentParser():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('-p', '--path', help='path to test  or test directory', default='./', type=str)
    parser.add_argument('-b', '--binary', help='path to impala binary', default='../build/bin/impala', type=str)
    parser.add_argument('-n', '--noCleanUp', action='store_true', default=False, dest='noCleanUp', help='keep log files after test run')
    parser.add_argument('-ct', '--compile-timeout', help='timeout for running test case',     default=10,             type=int)
    parser.add_argument('-rt', '--run-timeout', help='timeout for compiling project', default=5, type=int)
    args = parser.parse_args()
    return args


def setupTestSuit(args):
    categories = {}
    tests = []    
    
    if os.path.isfile(args.path):
        tests.append([args.path])
        return categories, tests

    if args.path=='./':
        actualDir = os.getcwd().split('/')[-1:][0]
        categories[0]=actualDir
    else: 
        underDir = args.path.split('/')[-1:][0]
        categories[0]=underDir    
    counter=1


    for subdir, dirs, files in os.walk(args.path):
        for dir in dirs:
            categories[counter]=dir
            counter+=1
        list = []
        for file in files:
            if (file[-7:]=='.impala'):
                list.append([os.path.join(subdir, file),file])
        tests.append(list)   
    return categories, tests        

def runTests(categories, tests, log):
    categorieCounter = 0
    totalTestCounter = 0
    totalSuccessCounter = 0
    totalTimeoutCounter = 0
    for testsuit in tests:
        sys.stdout.write('----------running Category ' + categories[categorieCounter] + '----------\n')
        testCounter=0
        successCounter=0
        timeoutCounter=0
        for test in testsuit:
            testCounter+=1
            sys.stdout.write('[' + test[1][:-7] + '] : ' ) 
            cmd = [args.binary]
            cmd.append(test[0])
            try:
                p = subprocess.run(cmd, stderr=log, stdout=log, timeout=args.run_timeout)
                if (p.returncode==0):
                    sys.stdout.write('SUCCESS\n')
                    successCounter+=1
                else:
                    sys.stdout.write('FAILED\n')
            except subprocess.TimeoutExpired as timeout:
                sys.stdout.write('Timed out after ' + str(args.run_timeout) + ' \n')
                timeoutCounter+=1            
        categorieCounter+=1
        totalTestCounter+=testCounter
        totalSuccessCounter+=successCounter
        totalTimeoutCounter+=timeoutCounter
        sys.stdout.write('Tests: ' + str(testCounter) + ' Passed: ' + str(successCounter) + ' Timed out: ' + str(timeoutCounter) + ' Failed: ' + str(testCounter-successCounter-timeoutCounter) + '\n\n')
    sys.stdout.write('Total >>  Tests: ' + str(totalTestCounter) + ' Passed: ' + str(totalSuccessCounter) + ' Timed out: ' + str(totalTimeoutCounter) + ' Failed: ' + str(totalTestCounter-totalSuccessCounter-totalTimeoutCounter) + '\n\n')


log = open('log', 'w')
args =  argumentParser()
categories, tests = setupTestSuit(args)
runTests(categories, tests, log)

if  not args.noCleanUp:
    subprocess.run(['rm','log']) 
