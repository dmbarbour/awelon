
import os
import sys
from .abcTypes import *

# abstract class for invocations and annotations
# note: sealers/unsealers are handled outside of invoker
class Invoker(object):
  def anno(self,tok,val): return val
  def invoke(self,tok,val): raise error('unknown power: ' + tok)

powerToken = '~power~'
powerBlock = B(True,True,[Inv(powerToken)])

# a very basic set of side-effects.
# might be useful to support python moduless?
class DefaultInvoker(Invoker):
  def invoke(self, tok, val):
    if (tok is powerToken): return P(powerBlock, self.power(val))
    else: raise error('unknown token: ' + tok)
  def power(self,val):
    assert isProd(val)
    args = val.r
    cmd = valToText(val.l)
    if  (u'readFile' == cmd): return readFile(args)
    elif(u'writeFile' == cmd): return writeFile(args)
    elif(u'randomBytes' == cmd): return randomBytes(args)
    elif(u'getOSEnv' == cmd): return getOSEnv(args)
    elif(u'destroy' == cmd): return destroy(args)
    elif(u'debugOut' == cmd): return debugOut(args)
    else: raise error('unknown command: ' + cmd)

def readFile(args): # args is text for filename
  fname = valToText(args)
  try: 
    fp = open(fname)
    txt = fp.read()
    fp.close()
    return R(textToVal(txt))
  except: return L(unit)

def writeFile(args): # args is pair (filename,text)
  fn = valToText(args.l)
  content = valToText(args.r)
  try:
    fp = open(fn,'w')
    fp.write(content)
    fp.close()
    return R(unit)
  except: 
    return L(unit)

def randomBytes(args): # args is integer, number
  count = int(args)
  assert(count >= 0),'invalid count for random bytes'
  bytes = os.urandom(count)
  val = L(unit)
  for b in bytes:
    val = R(P(number(ord(b)),val))
  return val

def destroy(args): # arg is object to be destroyed
  return unit

def getOSEnv(args): # args is environment variable
  envVar = valToText(args)
  envVal = os.getEnv(envVar,'')
  return textToVal(envVal)

def debugOut(args): # print args to stderr, then continue 
  sys.stderr.write(str(args)+'\n')
  return args
