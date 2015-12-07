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
        self.killed = False
        self.input_file = None

    def setInput(self, input_file):
        self.input_file = input_file

    def execute(self):
        def target():
            if self.input_file is None:
                self.process = subprocess.Popen(self.cmd, cwd=self.cwd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            else:
                self.input_file = open(self.input_file)
                self.process = subprocess.Popen(self.cmd, cwd=self.cwd, stdin=self.input_file, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
            self.output, _ = self.process.communicate()

        thread = threading.Thread(target=target)
        thread.start()
	
	#print self.cmd, self.cwd, self.input_file

        thread.join(self.timeout)
        if thread.is_alive():
            self.killed = True
            try:
                self.process.terminate()
                if self.input_file is not None:
                    self.input_file.close()
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

        if self.input_file is not None:
            self.input_file.close()

    def success(self):
        assert self.returncode is not None
        return self.returncode == 0
    
    def crash(self):
        assert self.returncode is not None
        return self.returncode < 0

class CompileProcess(TimedProcess):
    DEFAULT_TIMEOUT = 5.0
    timeout = DEFAULT_TIMEOUT
    
    def __init__(self, cmd, cwd, t=None):
        super(CompileProcess, self).__init__(cmd, cwd, t if t is not None else CompileProcess.timeout)

class RuntimeProcess(TimedProcess):
    DEFAULT_TIMEOUT = 5.0
    timeout = DEFAULT_TIMEOUT
    
    def __init__(self, cmd, cwd, timeout=timeout):
        super(RuntimeProcess, self).__init__(cmd, cwd, timeout)
