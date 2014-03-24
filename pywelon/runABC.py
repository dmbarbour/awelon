
import sys
import io
from .abcTypes import *
from .parseABC import *

# abstract class for invocations and annotations
class Invoker(object):
  def anno(self,tok,val): return val
  def invoke(self,tok,val): raise error('unknown power: ' + tok)

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
  pb  = B(False,False,(Inv('~power~'),))
  sn  = textToVal("")
  rns = unit
  ex  = P(P(sn,rns),unit)
  stdenv = P(s,P(h,P(pb,ex)))
  return stdenv

def defaultInvoker():
  return unit

def main(): 
  finalEnv = runABC(streamSource(sys.stdin),defaultEnv(), defaultInvoker())
  printEnvSummary(finalEnv)

# Python's semantic hack for modules as programs.
if __name__ == "__main__": main()
