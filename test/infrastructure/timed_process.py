'''
Created on 1 Jul 2014

@author: David Poetzsch-Heffter
'''

import subprocess, threading, errno

class TimedProcess(object):
    def __init__(self, cmd, cwd, timeout):
        self.cmd = cmd
        self.cwd = cwd
        self.timeout = timeout
        self.process = None
        self.output = ""
        self.returncode = None

    def execute(self):
        def target():
            self.process = subprocess.Popen(self.cmd, cwd=self.cwd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            self.output, _ = self.process.communicate()

        thread = threading.Thread(target=target)
        thread.start()

        thread.join(self.timeout)
        if thread.is_alive():
            try:
                self.process.terminate()
                thread.join(self.timeout / 10.0)
                if thread.is_alive():
                    self.process.kill()
                    thread.join()
            except OSError as e:
                if e.errno != errno.ESRCH: # error is not "no such process"
                    raise e
        
        self.process.poll()
        assert self.process.returncode is not None
        self.returncode = self.process.returncode

    def success(self):
        assert self.returncode is not None
        return self.returncode == 0

class CompileProcess(TimedProcess):
    timeout = 1.0
    
    def __init__(self, cmd, cwd):
        super(CompileProcess, self).__init__(cmd, cwd, CompileProcess.timeout)
