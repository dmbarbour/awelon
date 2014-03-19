
import sys
import io
from .abcTypes import *


# runABC takes a 'source' (for the program) and an
# 'environment' (an initial value). It returns the
# final environment value, after processing the
# entire source.
#
# To consider:
#   shift this into a class or object
#   such that the env & invoker is part of the object
def runABC(source, env, invoker):
  print "runABC"


# defaultEnv will return the default initial environment.
# for now, this is just a constant. However, I'd like to
# also support persistent computations.
def defaultEnv(): return aoStdEnv()
def aoStdEnv():
  s   = unit
  h   = unit
  pb  = block([invocation("~power~")])
  sn  = textToVal("")
  rns = unit
  ex  = unit
  stdenv = P(s,P(h,P(pb,P(P(sn,rns),ex))))
  return stdenv

def defaultInvoker():
  return unit

def main(): 
  finalEnv = runABC(streamSource(sys.stdin),defaultEnv(), defaultInvoker())
  printEnvSummary(finalEnv)

# Python's semantic hack for modules as programs.
if __name__ == "__main__": main()
