
import os
from .abcTypes import *

# abstract class for invocations and annotations
class Invoker(object):
  def anno(self,tok,val): return val
  def invoke(self,tok,val): raise error('unknown power: ' + tok)

powerToken = '~power~'
powerBlock = B(True,True,[Inv(powerToken)])

class DefaultInvoker(Invoker):
  def invoke(self, tok, val):
    if (tok is powerToken): return P(powerBlock, self.power(val))
    else: raise error('unknown power: ' + tok)
  def power(self,val):
    assert isProd(val)
    args = val.r
    cmd = valToText(val.l)
    if(u'readFile' == cmd): return self.readFile(args)
    elif(u'writeFile' == cmd): return self.writeFile(args)
    elif(u'randomBytes' == cmd): return self.randomBytes(args)
    elif(u'getOSEnv' == cmd): return self.getOSEnv(args)
    elif(u'destroy' == cmd): return self.destroy(args)
    elif(u'debugOut' == cmd): return self.debugOut(args)
    else: raise error('unknown command: ' + cmd)
  def readFile(self,args): # args is text for filename
    fname = valToText(args)
    try: fp = open(fname)
         txt = fp.read()
         fp.close()
         return R(textToVal(txt))
    except: return L(unit)
  def writeFile(self,args): # args is pair (filename,text)
    fn = valToText(args.l)
    content = valToText(args.r)
    try:
      fp = open(fn,'w')
      fp.write(content)
      fp.close()
      return R(unit)
    except: 
      return L(unit)
  def randomBytes(self, args): # args is integer, number
    count = int(args)
    assert(count >= 0),'invalid count for random bytes'
    bytes = os.urandom(count)
    return bytesToList(bytes)
  def destroy(self,args): # arg is object to be destroyed
    return unit
  def getOSEnv(self,args): # args is environment variable
    envVar = valToText(args)
    envVal = os.getEnv(envVar,'')
    return textToVal(envVal)
  def debugOut(self,args): # print args then continue 
    print args
    return args
