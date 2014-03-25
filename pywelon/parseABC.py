# Parse ABC text into code.

from .abcTypes import *

# parseABC receives text (str or unicode) and outputs a tuple
# of code elements (Inv, Op, Lit). 
def parseABC(src):
  assert (type(src) is str)
  loc,code = stepParseABC(0,src)
  assert (loc == len(src))
  return code

abcOps = 'lrwzvcLRWZVC%^$kfo\'+*/-Q?DFMK>\n #0123456789'

# stepParseABC receives a location to parse from, and the ABC code. 
def stepParseABC(loc,src):
  codes = []
  while(loc < len(src)):
    op = s[loc]
    if((' ' == op) or ('\n' == op)): loc += 1
    elif op in abcOps: 
       codes.append(ord(op))
       loc += 1
    elif ('[' == op):
       loc,body = stepParseABC(loc+1,s) 
       assert isEndOfBlock(loc,s)
       loc += 1
       codes.append(Lit(B(False,False,body)))
    elif (']' == op):
       return loc,codes
    elif ('"' == op):
       loc,text = parseText(loc,s)
       assert isEndOfText(loc,s)
       loc += 1
       codes.append(Lit(textToVal(text))
    elif ('{' == op):
       loc,tok = parseInvocation(loc,s)
       assert isEndOfInvoke(loc,s)
       loc += 1
       codes.append(Inv(tok))
    else: raise error('unknown operator: ' ++ op)
  return loc,codes

def parseInvocation(loc,src):
  assert('{' == src[loc])
  pc = loc+1
  while(not ('}' == src[pc])):
    assert isValidTokChr(src[pc])
    pc += 1
  return pc,src[(loc+1):pc]

def isValidTokChr(c): return not (('\n' == c) or ('{' == c) or ('}' == c))

def parseText(loc,src):
  assert('"' == src[loc])
  result = ''
  loc += 1
  while True:
    eol = src.index('\n',loc)
    assert((eol >= loc) and ('\n' == src[eol]))
    if(' ' == src[eol+1]): # SP escapes prior LF
      result = result + src[loc:(eol+1)] # includes LF
      loc = eol+2 # excludes SP
    else:
      assert ('~' == src[eol+1]) # ABC string terminator
      return (eol+1),(result + src[loc:eol])

def isEndOfBlock(loc,s): return (loc < len(s)) and (']' == s[loc])
def isEndOfInvoke(loc,s): return (loc < len(s)) and ('}' == ord(s[loc]))
def isEndOfText(loc,s):
  return (loc < len(s)) and (loc>0) \
     and (s[loc] == '~') and (s[loc-1] == '\n')





