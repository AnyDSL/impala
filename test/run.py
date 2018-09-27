#!/usr/bin/env python

# - make output nicer (better error messages)

import os
import argparse
import sys
import subprocess
import filecmp
import difflib
import threading
from collections import namedtuple
import time

# more constants here
UNHANDLED = -1
PASSED = 0
CLANG_FAILED = 1
CLANG_TIMEOUT = 2
IMPALA_FAILED = 3
IMPALA_TIMEOUT = 4
RUN_FAILED = 5
RUN_TIMEOUT = 6
OUTPUT_DIFFER = 7
LOG_DIFFER = 8

POSITIVE = [PASSED]
NEGATIVE = [CLANG_FAILED, IMPALA_FAILED, RUN_FAILED, OUTPUT_DIFFER, LOG_DIFFER]
TIMEOUT = [CLANG_TIMEOUT, IMPALA_TIMEOUT, RUN_TIMEOUT]

class test:
    name=''
    path=''
    result = UNHANDLED

    def __init__(self,test_path, test_name):
        self.name = test_name
        self.path = test_path

    def __lt__(self, other):
        return self.name < other.name

    def ___le__(self, other):
        return self.name <= other.name

    def __eq__(self, other):
        return self.name == other.name

    def __ne__(self, other):
        return self.name != other.name

    def __gt__(self, other):
        return self.name > other.name

    def __ge__(self, other):
        return self.name >= other.name

def parse_args():
    parser = argparse.ArgumentParser(formatter_class = argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('path', nargs='*',          help='path to test  or test directory',      default='./', type=str)
    parser.add_argument('-c',  '--clang',           help='path to clang binary',                 default=None, type=str)
    parser.add_argument('-i',  '--impala',          help='path to impala binary',                default=None, type=str)
    parser.add_argument('-it', '--impala-timeout',  help='timeout for compiling impala ',        default=5,    type=int)
    parser.add_argument('-ct', '--clang-timeout',   help='timeout for compiling  clang',         default=5,    type=int)
    parser.add_argument('-rt', '--run-timeout',     help='timeout for running binary',           default=10,   type=int)
    parser.add_argument('-j', '--concurrency',      help='numbers of threads to use',            default=1,    type=int)
    parser.add_argument('-b',  '--broken',          help='also run broken tests',                default=False, action='store_true', dest='broken')
    parser.add_argument('-n',  '--no-clean_up',     help='keep log files after test run',        default=False, action='store_true', dest='noclean_up')
    parser.add_argument('-l',  '--logfile',         help='create non existing logfiles',         default=False, action='store_true', dest='logfile')
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
    entry = test(file, testname)
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

def compare_files(a, b): # True if equal, false otherwise
    if os.path.isfile(b):
        return filecmp.cmp(a, b)
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

def analyze_returncode(returncode):
    if returncode < 0:
        return (False, 'execution was terminated by signal {}'.format(-returncode))
    if returncode > 0:
        return (False, "execution didn't run successfully and return exit code {}".format(returncode))
    return (True, '')

def run_tests():
    def worker(testsuit):
        def run_test():
            test_path = test.path
            test_name = test.name
            tmp_log   = test_name +'.tmp.log'
            tmp_exe   = test_name
            tmp_ll    = test_name + '.ll'
            tmp_out   = test_name + '.tmp.out'

            def run_codegen_test():
                def create_logfile():
                    if os.path.isfile(orig_log):
                        return
                    if os.path.isfile(tmp_log) and os.path.getsize(tmp_log) > 0:
                        subprocess.run(['cp', tmp_log, orig_log])

                orig_impala = test_path
                orig_in     = test_path[:-7] + '.in'
                orig_out    = test_path[:-7] + '.out'
                orig_log    = test_path[:-7] + '.log'
                error      = '\n---> '

                clang_args, exec_args = split_arguments(arguments)
                tmp_log_file = open(tmp_log, 'w')

                # invoke impala
                cmd_impala = [args.impala,orig_impala, '-emit-llvm', '-O2']

                try:
                    p = subprocess.run(cmd_impala, stderr=tmp_log_file, stdout=tmp_log_file, timeout=args.impala_timeout)
                except subprocess.TimeoutExpired as timeout:
                    error += 'impala time out'
                    return (IMPALA_TIMEOUT, error)
                except:
                    error += 'impala failed'
                    return (IMPALA_FAILED, error)

                (passed, msg) = analyze_returncode(p.returncode)
                if not passed:
                    error += 'impala ' + msg
                    return (IMPALA_FAILED, error)

                # invoke clang
                try:
                    cmd_clang = [args.clang, tmp_ll, 'lib.c', '-o', tmp_exe]
                    cmd_clang.extend(clang_args)
                    p = subprocess.run(cmd_clang, stderr=tmp_log_file, stdout=tmp_log_file, timeout=args.clang_timeout)
                except subprocess.TimeoutExpired as timeout:
                    error += 'clang time out'
                    return (CLANG_TIMEOUT, error)
                except:
                    error += 'clang failed'
                    return (CLANG_FAILED, error)

                tmp_log_file.close()

                # execute
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
                    error += 'execution time out'
                    return (RUN_TIMEOUT, error)
                except:
                    error += 'execution failed'
                    return (RUN_FAILED, error)
                tmp_out_file.close()

                (passed, msg) = analyze_returncode(p.returncode)
                if not passed:
                    error += 'execution ' + msg
                    return (RUN_FAILED, error)

                # log file
                if (args.logfile):
                    create_logfile()

                if not compare_files(tmp_log, orig_log):
                    error += 'log files differed'
                    return (LOG_DIFFER, error)

                # out file
                if not compare_files(tmp_out, orig_out):
                    error += 'outputs did not match:\n'
                    try:
                        with open(orig_out) as orig_out_file:
                            orig_lines = orig_out_file.readlines()
                        with open(tmp_out) as tmp_out_file:
                            tmp_lines = tmp_out_file.readlines()
                    except:
                        error += '(this is a binary output)'
                        return (RUN_FAILED, error)

                    diff = difflib.context_diff(orig_lines, tmp_lines, fromfile=orig_log, tofile=tmp_log)
                    error += 'outputs differ:\n' + ''.join(list(diff))
                    return (RUN_FAILED, error)

                return (PASSED, '')

            # run_test
            firstLine = read_first_line(test_path)
            broken, arguments = is_broken(firstLine)
            if (not args.broken) and broken:
                return

            arguments = arguments[1:]
            (test.result, output) = run_codegen_test()
            output = '{} {}{}'.format('passed' if test.result == PASSED else 'FAILED', test_path, output)
            print(output)

            # remove tmp files
            if not args.noclean_up:
                subprocess.run(['rm', '-f', tmp_ll])
                subprocess.run(['rm', '-f', tmp_out])
                subprocess.run(['rm', '-f', tmp_log])
                subprocess.run(['rm', '-f', tmp_exe])

        # worker
        global job_counter
        job_bound = len(testsuit)
        while(True):
            lock.acquire()
            job_number = job_counter
            job_counter +=1
            lock.release()
            if job_number >= job_bound:
                return
            test = testsuit[job_number]
            run_test()

    # run_tests
    categorie_counter = 0
    total_failed_counter = 0
    total_test_counter = 0
    total_success_counter = 0
    total_timeout_counter = 0

    executable = ['codegen']
    for e in executable:
        index = categories[e]
        testsuit = tests[index]
        threads = []
        lock = threading.Lock()
        sys.stdout.write('----------running Category ' + e + '----------\n')
        for i in range(args.concurrency):
            threads.append(threading.Thread(target = worker, args=(testsuit,)))
            threads[i].start()

        for i in range(args.concurrency):
            threads[i].join()

        for test in testsuit:
            if test.result in POSITIVE:
                total_success_counter += 1
            if test.result  in NEGATIVE:
                total_failed_counter += 1
            if test.result in TIMEOUT:
                total_timeout_counter +=1
            if test.result != UNHANDLED:
                total_test_counter +=1
    sys.stdout.write('>>> Total:    {}\n'.format(total_test_counter))
    sys.stdout.write('>>> Passed:   {}\n'.format(total_success_counter))
    sys.stdout.write('>>> Time out: {}\n'.format(total_timeout_counter))
    sys.stdout.write('>>> Failed:   {}\n'.format(total_failed_counter))

args =  parse_args()

impala = find_impala()
args.impala = impala

clang = find_clang()
args.clang = clang

categories, tests = set_up_test_suit()

start = time.time()
job_counter = 0
run_tests()
end = time.time()
passed_time = end - start
print('time for testing: ' + str(passed_time) + ' seconds')
