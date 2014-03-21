# Parse ABC text into code.

from .abcTypes import *

# parseABC receives text (str or unicode) and outputs a tuple
# of code elements (Inv, Ops, Lit). 
def parseABC(s):
  loc,code = stepParseABC(0,s)
  assert (loc == len(s)), 'failed to parse full ABC input'
  return code

abcOps = 'lrwzvcLRWZVC%^$kfo\'+*/-Q?DFMK>\n #0123456789'

# stepParseABC receives a location to parse from, and the ABC code. 
def stepParseABC(loc,s):
  codes = []
  while(loc < len(s)):
    op = s[loc]
    if((' ' == op) or ('\n' == op)): loc += 1
    elif op in abcOps: codes.append(ord(op)); loc += 1
    elif ('[' == op):
       loc,body = stepParseABC(loc+1,s) 
       assert isEndOfBlock(loc,s)
       loc += 1
       codes.append(Lit(B(False,False,body)))
    elif (']' == op):
       return loc,tuple(codes)
    elif ('"' == op):
       loc,text = parseText(loc+1,s)
       assert isEndOfText(loc,s)
       loc += 1
       codes.append(Lit(textToVal(text))
    elif ('{' == op):
       loc,tok = parseInv(loc+1,s)
       assert isEndOfInvoke(loc,s)
       loc += 1
       codes.append(Inv(tok))
    else: raise error('unknown operator: ' ++ op

def isEndOfBlock(loc,s): return (loc < len(s)) and (']' == s[loc])
def isEndOfInvoke(loc,s): return (loc < len(s)) and ('}' == ord(s[loc]))
def isEndOfText(loc,s):
  return (loc < len(s)) and (loc>0) \
     and (s[loc] == '~') and (s[loc-1] == '\n')
    
