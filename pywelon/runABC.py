
import sys
import io
from .abcTypes import *
from .parseABC import *
from .effects import *
    


# runABC takes a 'source' (for the program) and an
# 'environment' (an initial value). It returns the
# final environment value, after processing the
# entire source.
#
# To consider:
#   shift this into a class or object
#   such that the env & invoker is part of the object
def runABC(source, env, invoker): 
  return runParsedABC(parseABC(code),env,invoker)

def runParsedABC(code, env, invoker):
  pc = 0
  while(pc < len(code)):
    op = code[pc]
    if(type(co

def printEnvSummary(env): 
  print env # should aim to do a better job, here!

# defaultEnv will return the default initial environment.
# for now, this is just a constant. However, I'd like to
# also support persistent computations.
def aoStdEnv():
  s   = unit
  h   = unit
  pb  = powerBlock
  sn  = textToVal("")
  rns = unit
  ex  = P(P(sn,rns),unit)
  stdenv = P(s,P(h,P(pb,ex)))
  return stdenv

def defaultInvoker: return DefaultInvoker()
def defaultEnv(): return aoStdEnv()
def defaultProgram(): return sys.stdin.read()

def main(): 
  finalEnv = runABC(defaultProgram(), defaultEnv(), defaultInvoker())
  printEnvSummary(finalEnv)

# Python's semantic hack for modules as programs.
if __name__ == "__main__": main()
