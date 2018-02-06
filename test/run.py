#!/usr/bin/env python

# - remove code duplication
# - camal_case 
# - three time outs: impala/clang/exec - done
# - more fail constants 
# - make output nicer (better error messages)
# - parallelize: -j (std: num cpu cores)
# # diff output if compare_Files don't match (not for binary)

import os
import argparse
import sys
import subprocess
import filecmp
import difflib

# more constants here
SUCCESS = 0
CLANG_FAILED = 1
CLANG_TIMEDOUT = 2
IMPALA_FAILED = 3
IMPALA_TIMEDOUT = 4
RUN_FAILED = 5
RUN_TIMEOUT = 6

def argumentParser():
    parser = argparse.ArgumentParser(formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('path', nargs='*',          help='path to test  or test directory',      default='./', type=str)
    parser.add_argument('-c',  '--clang',           help='path to clang binary',                 default=None, type=str)
    parser.add_argument('-i',  '--impala',          help='path to impala binary',                default=None, type=str)
    parser.add_argument('-it', '--impala-timeout', help='timeout for compiling impala ', default=5,    type=int)
    parser.add_argument('-ct', '--clang-timeout', help='timeout for compiling  clang', default=5,    type=int)
    parser.add_argument('-rt', '--run-timeout',     help='timeout for running binary',           default=10,   type=int)
    parser.add_argument('-b',  '--broken',          help='also run broken tests',                default=False, action='store_true', dest='broken')
    parser.add_argument('-n',  '--no-cleanup',      help='keep log files after test run',        default=False, action='store_true', dest='noCleanUp')
    args = parser.parse_args()
    return args

def find_impala():
    if args.impala != None:
        return args.impala
    p = subprocess.Popen(['printenv', 'PATH'], stdout = subprocess.PIPE)
    (out, err) = p.communicate()
    sout = str(out)[2:-3]
    list = sout.split(':')

    for dir in list:
        bin = dir + '/impala'
        if os.path.isfile(bin):
            return bin

    return '../build/bin/impala'

def find_clang():
    if args.clang != None:
        return args.clang
    p = subprocess.Popen(['printenv', 'PATH'], stdout=subprocess.PIPE)
    (out, err) = p.communicate()
    sout = str(out)[2:-3]
    list = sout.split(':')

    for dir in list:
        bin = dir + '/clang'
        if os.path.isfile(bin):
            return bin

    return 'clang'

def read_first_line(file):
    with open(file) as rfile:
        line = rfile.readline()
        if line[:2] != '//':
            return None
        return line[2:].split()

def is_broken(X):
    for x in X:
        if (x == 'broken'):
            res = []
            for y in X:
                if y != x:
                    res.append(y)
            return True, res
    return False, X

def give_categorie(categories, file):
    line = read_first_line(file) 
    if line == None:
        return 0
    if line[0] in categories:
        return categories[line[0]] 
    return 0

def sort_in(categories, tests, file):
    cat = give_categorie(categories, file)
    testpath = file.split('/')
    testname = testpath[-1][:-7]
    entry = [file, testname]
    return cat, entry

def set_up_test_suit():
    categories = {}
    categories['undefined']=0
    categories['codegen']=1
    categories['sema']=2
    categories['type_inferr']=3
    tests = [[],[],[],[]]    
    
    if args.path == []:
        args.path.append('./')

    for x in args.path:
        if os.path.isfile(x):
            (cat, entry) = sort_in(categories, tests, x)
            tests[cat].append(entry)
            continue

        for subdir, dirs, files in os.walk(x):
            for file in files:
                if (file[-7:] == '.impala'):
                    cat, entry = sort_in(categories, tests, os.path.join(subdir, file))
                    tests[cat].append(entry)
    sorted_tests = []  
    for t in tests:
        sorted_tests.append(sorted(t))             
    return categories, sorted_tests 

def compare_Files(tmp_out, out): # True if equal, false otherwise
    if os.path.isfile(out):
        return filecmp.cmp(tmp_out, out)
    else: 
        return True

def split_arguments(arguments):
    clang_args = []
    exec_args = []
    for argument in arguments:
        if argument[0] == '-':
            clang_args.append(argument)
        else:
            exec_args.append(argument[1:-1])
    return clang_args, exec_args


def runTests():
    def runCodegenTest():
        def cleanUp():
            if not args.noCleanUp:
                subprocess.run(['rm', tmp_ll])
                subprocess.run(['rm', tmp_out])
                subprocess.run(['rm', tmp_log])
                subprocess.run(['rm', tmp_exe])

        orig_impala = test_path
        orig_in     = test_path[:-7] + '.in'
        orig_out    = test_path[:-7]+'.out'
        
        tmp_log = test_name +'.tmp.log'
        tmp_exe = test_name
        tmp_ll  = test_name + '.ll'
        tmp_out = test_name+'.tmp.out'

        clang_args, exec_args = split_arguments(arguments)
        cmd_impala = [args.impala]
        cmd_impala.append(orig_impala)
        cmd_impala.append('-emit-llvm')

        try:
            tmp_log_file = open(tmp_log, 'w')
            p = subprocess.run(cmd_impala, stderr=tmp_log_file, stdout=tmp_log_file, timeout=args.impala_timeout)
            if p.returncode != 0:
                return IMPALA_FAILED
        except subprocess.TimeoutExpired as timeout:
            return IMPALA_TIMEDOUT    
        except:   
            return IMPALA_FAILED
      
        try:          
            cmd_clang = [args.clang, tmp_ll, 'lib.c', '-o', tmp_exe]
            cmd_clang.extend(clang_args)
            p = subprocess.run(cmd_clang,  stderr=tmp_log_file, stdout=tmp_log_file, timeout=args.clang_timeout)
        except subprocess.TimeoutExpired as timeout:
            return CLANG_TIMEDOUT  
        except:
            return CLANG_FAILED      

        cmd_exec = ['./' + tmp_exe]
        cmd_exec.extend(exec_args)
        try:
            orig_in_file = open(orig_in)
        except:
            orig_in_file = None
        tmp_out_file = open(tmp_out, 'w') 

        try:
            p = subprocess.run(cmd_exec, stdin = orig_in_file, stdout=tmp_out_file, timeout=args.run_timeout)
        except subprocess.TimeoutExpired as timeout:
            return RUN_TIMEOUT 
        except:
            return RUN_FAILED    
        tmp_out_file.close()
        diff = compare_Files(tmp_out, orig_out)

        if diff:
            cleanUp()
            return SUCCESS

        sys.stdout.write('Output did not match expectation:\n')
        with open(orig_out) as orig_out_file:
            orig_lines = orig_out_file.readlines()
        with open(tmp_out) as tmp_out_file:
            tmp_lines = tmp_out_file.readlines()
            
        orig_length = len(orig_lines)
        tmp_length  = len(tmp_lines)
        
        for i in range(orig_length):
            orig_line = orig_lines[i]
            tmp_line = tmp_lines[i]
            if (orig_line != tmp_line):
                difference = ''.join(difflib.ndiff(orig_line, tmp_line))
                sys.stdout.write('line ' + str(i) +': ' + difference)
        cleanUp()
        return RUN_FAILED

    categorie_Counter = 0
    total_test_counter = 0
    total_success_counter = 0
    total_timeout_counter = 0
    executable = ['codegen']
    for exec in executable:
        index = categories[exec]
        testsuit = tests[index]
        sys.stdout.write('----------running Category ' + exec + '----------\n')
        test_counter = 0
        success_counter = 0
        timeout_counter = 0
        for test_path, test_name in testsuit:
            firstLine = read_first_line(test_path)
            broken, arguments = is_broken(firstLine)
            if  (not args.broken) and broken:
                continue
            arguments = arguments[1:]    
            sys.stdout.write('[' + test_name + '] : ' )
            test_counter += 1
            x = runCodegenTest()
            if x == SUCCESS:
                success_counter += 1
                sys.stdout.write('passed\n')
                continue
            if x == IMPALA_TIMEDOUT:
                timeout_counter += 1
                sys.stdout.write('Impala timed out\n')
                continue
            if x == CLANG_TIMEDOUT:
                timeout_counter += 1
                sys.stdout.write('Clang timed out\n')
                continue
            if x == RUN_TIMEOUT:
                timeout_counter += 1
                sys.stdout.write('Binary timed out\n')
                continue
            if x == IMPALA_FAILED:
                sys.stdout.write('Impala failed\n') 
            if x == CLANG_FAILED:
                sys.stdout.write('Clang failed\n') 
            if x == RUN_FAILED:
                sys.stdout.write('Binary failed\n')        
        categorie_Counter += 1
        total_test_counter += test_counter
        total_success_counter += success_counter
        total_timeout_counter += timeout_counter
        sys.stdout.write('Tests: ' + str(test_counter) + ' Passed: ' + str(success_counter) + ' Timed out: ' + str(timeout_counter) + ' Failed: ' + str(test_counter - success_counter - timeout_counter) + '\n\n')
    sys.stdout.write('>>>>>Total>>>>  Tests: ' + str(total_test_counter) + ' Passed: ' + str(total_success_counter) + ' Timed out: ' + str(total_timeout_counter) + ' Failed: ' + str(total_test_counter - total_success_counter - total_timeout_counter) + '\n\n')


log = open('log', 'w')
args =  argumentParser()

impala = find_impala()
args.impala = impala

clang = find_clang()
args.clang = clang
 

categories, tests = set_up_test_suit()
runTests()
