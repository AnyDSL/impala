#!/usr/bin/env python

# - remove code duplication
# - make output nicer (better error messages)
# - parallelize: -j (std: num cpu cores)

import os
import argparse
import sys
import subprocess
import filecmp
import difflib
import threading
import random
import time

# more constants here
SUCCESS = 0
CLANG_FAILED = 1
CLANG_TIMEDOUT = 2
IMPALA_FAILED = 3
IMPALA_TIMEDOUT = 4
RUN_FAILED = 5
RUN_TIMEOUT = 6
OUTPUT_DIFFER = 7
LOG_DIFFER = 8




def test():
    def inc():
        lock.acquire()
        t = a
        time.sleep(i)
        t += 1
        a = t
        lock.release()

    lock = threading.Lock()
    a = 0
    t = []
    for i in range(10):
        t.append(threading.Thread(target = inc))
        t[i].start()

    for i in range(10):
        t[i].join()
    print(a)


test()