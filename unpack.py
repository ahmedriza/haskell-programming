import sys
import shlex
from subprocess import Popen, PIPE

def deps():
        args = shlex.split("stack list-dependencies --test --bench")
        process = Popen(args, stdout=PIPE, stderr=PIPE)
        out, err = process.communicate()
        exitcode = process.wait()
        if err != '':
                print "There was an error from stack list-dependencies: ", err
                sys.exit(-1)
        dependencyList = out.strip().split('\n') # e.g. ['HUnit 1.5.0.0', 'JuicyPixels 3.2.8.3']
        dependencies = [parseDep(d) for d in dependencyList]
        for dep  in dependencies:
                unpackDep(dep)

def unpackDep(dep): # argument is of the form 'HUnit-1.5.0.0'
        args = shlex.split("stack unpack " + dep)
        process = Popen(args, stdout=PIPE, stderr=PIPE)
        out, err = process.communicate()
        exitcode = process.wait()

def parseDep(dep):
        parts = dep.split()
        return parts[0] + "-" + parts[1]

if __name__ == '__main__':
	deps()
