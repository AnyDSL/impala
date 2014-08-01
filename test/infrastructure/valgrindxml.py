'''
Created on 1 Aug 2014

@author: David Poetzsch-Heffter
'''

import xml.etree.ElementTree as ET

class ValgrindXML(object):
    def __init__(self, file):
        self.root = ET.parse(file)
        self.leaks = []

        for err in self.root.findall("./error"):
            kind = err.find("./kind")
            if kind.text.startswith("Leak_"):
                self.leaks.append(err)
    
    def __repr__(self):
        s = "VALGRIND HEAP SUMMARY:\n\n"
        
        for leak in self.leaks:
            s += leak.find("./xwhat/text").text + "\n"
            
            r = "at"
            for frame in leak.findall("./stack/frame"):
                ip = frame.find("ip").text
                fn = frame.find("fn").text
                
                f = frame.find("file")
                if f is not None:
                    loc = f.text + ":" + frame.find("line").text
                else:
                    loc = frame.find("obj").text
                
                s += "   %s %s: %s (in %s)\n" % (r, ip, fn, loc)
                r = "by"
            
            s += "\n"
        
        return s
    
if __name__ == "__main__":
    # TESTING
    import tests
    print ValgrindXML(tests.ValgrindTest.VALGRIND_XML_FILE)