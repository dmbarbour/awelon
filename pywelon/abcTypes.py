
# Types for Awelon Bytecode (ABC)
#
# Value Types
#   U - empty class acts as value
#   P - named tuple for products
#   L,R - named tuples for sums
#   B - blocks, named tuple with 'affine', 'relevant', 'code'.
#   SV - sealed values, with sealer 's' and value 'v'
#   float - current implementation of numbers (might switch to fractions)
#
# Code Types:
#   int - ABC operator
#   Inv - invocations contain a string {foo}
#   Lit - literals contain a value type (e -> (L*e))
#
# Todo: Find an alternative model for code and values to respect RPython's
#  constraint on variables having a single type. Maybe code as composition
#  of functions? 
#
# Code and values should not bind to the Python environment, at least
# not strongly. E.g. blocks can be serialized to ABC and recovered.
#

from collections import namedtuple

# ABC types in Python:
P = namedtuple('P',('l','r'))
def isProd(x): return (type(x) is P)

L = namedtuple('L',('inL',))
R = namedtuple('R',('inR',))
def isL(x): return (type(x) is L)
def isR(x): return (type(x) is R)
def isSum(x): return (isL(x) or isR(x))
def assertSum(x): assert isSum(x), "sum expected; received %r" % (x,)

class U(tuple):
  def __init__(self): 
    assert False, "unit is the class; do not instantiate!"
unit = U
def isUnit(x): return (x is U)
def assertUnit(x): assert isUnit(x), "unit expected; received %r" % (x,)

SV = namedtuple('SV',('s','v'))
def isSealedVal(x): return (type(x) is SV)
def assertSealedVal(x): 
  assert isSealedVal(x), \
    "expecting sealed val; received %r" % (x,)
def assertSealedBy(x,s): 
  assert isSealedVal(x) and (s == x.s),\
    "expecting value sealed with %r; received %r" % (s,x)

def unseal(s,x): assertSealedBy(x,s); return x.v

def number(x): return float(x)
def isNumber(x): return (type(x) is float)
def assertNumber(x): assert isNumber(x), "number (float) expected; received %r" % (x,)
def add(n1,n2): return (n1 + n2)
def neg(n1): return (- n1)
def mul(n1,n2): return (n1 * n2)
def inv(n1): return (1/n1)
def divQ(n1,n2): return divmod(n1,n2)  # n1/n2 -> (q,r)
def addDigit(n1,nd): return add(mul(n1,number(10)),number(nd))


def textToVal(s): 
  if type(s) is str: return uTextToVal(s.decode('utf-8'))
  elif type(s) is unicode: return uTextToVal(s)
  else: raise error('unknown text type: ' + type(s))
def uTextToVal(s):
  assert (type(s) is unicode), "expecting unicode, got %r" % type(s)
  t = L(unit)
  for c in reversed(s):
    n = ord(c)
    if((0xD800 <= c) and (c <= 0xDFFF)):
       raise error("TODO: support surrogate pairs or replace utf-8 decoder")
    t = R(P(number(ord(c)),t))
  return t

# tryValToText will return a string or None
#   apparently the Python idiom is to use 
#   failure as a quick escape, so I'll do that
def tryValToText(v):
  try: return valToText(v)
  except: return None

def valToText(v):
  chars = []
  while(isR(v)):
    chars.append(unichr(int(v.inR.l)))
    v = v.inR.r
  return u''.join(chars)

# Blocks are very simple in pywelon:
#   affine and relevant are boolean
#   code is a list of int/Lit/Inv values
#     this list should be considered immutable
#     (pypy doesn't support variable-length tuples)
B = namedtuple('B', ('affine','relevant','code'))

def isBlock(b): return type(b) is B
def compose(b1,b2):
  aff = b1.affine or b2.affine
  rel = b1.relevant or b2.relevant
  code = composeCode(b1.code,b2.code)
  return B(aff,rel,code)
def composeCode(c1,c2):
  return (c1 + c2) # no simplification for now


# Code types for ABC operators
Lit = namedtuple('Lit',('val',)) # literal value (e -> (L*e))
Inv = namedtuple('Inv',('tok',)) # {token}

def isOp(x):  return type(x) is int
def isLit(x): return type(x) is Lit
def isInv(x): return type(x) is Inv
 

# simple operations on values
def op_w(v): return P(v.r.l, P(v.l, v.r.r))
def op_l(v): return P(P(v.l, v.r.l), v.r.r)
def op_r(v): return P(v.l.l, P(v.l.r, v.r))
def op_z(v): return P(v.l,P(v.r.r.l,P(v.r.l,v.r.r.r)))
def op_v(v): return P(v,unit)
def op_c(v): assertUnit(v.r); return v.l

def op_W(v):
  s = v.l
  if(isL(s)): s = R(s) 
  elif(isL(s.inR)): s = s.inR
  else: pass
  return P(s,v.r)
def op_L(v):
  s = v.l
  if(isL(s)): s = L(s) 
  elif(isL(s.inR)): s = L(R(s.inR.inL))
  else: s = s.inR
  return P(s,v.r)
def op_R(v):
  s = v.l
  if(isR(s)): s = R(s)
  elif(isR(s.inL)): s = R(L(s.inL.inR))
  else: s = s.inL
  return P(s,v.r)
def op_Z(v):
  s = v.l
  if(isL(s)): pass
  elif(isL(s.inR)): s = R(s)
  elif(isL(s.inR.inR)): s = s.inR 
  else: pass
  env = P(s,v.r)
def op_V(v): return P(L(v.l),v.r)
def op_C(v): return P(v.l.inL,v.r)

copyDropTestDepth = 4 # to keep copy, drop as O(1) over large structures
def op_drop(v): assert droppable(v.l,copyDropTestDepth); return v.r
def op_copy(v): assert copyable(v.l,copyDropTestDepth); return P(v.l,v)

# test whether a value is copyable up to given depth (on pairs)
def copyable(x,N): 
  if((N < 1) or isNumber(x) or isUnit(x)): return True
  elif isProd(x): return (copyable(x.l,N-1) and copyable(x.r,N-1))
  elif isL(x): return copyable(x.inL,N)
  elif isR(x): return copyable(x.inR,N)
  elif isBlock(x): return (not x.affine)
  elif isSealedVal(x): return copyable(x.v,N)
  else: raise error("invalid value %r passed to 'copyable'" % (x,))

# test whether a value is droppable up to a given depth (on pairs)
def droppable(x,N):
  if ((N < 1) or isNumber(x) or isUnit(x)): return True
  elif isProd(x): return (droppable(x.l,N-1) and droppable(x.r,N-1))
  elif isL(x): return droppable(x.inL,N)
  elif isR(x): return droppable(x.inR,N)
  elif isBlock(x): return (not x.relevant)
  elif isSealedVal(x): return droppable(x.v,N)
  else: raise error("invalid value %r passed to 'droppable'" % (x,))


def op_comp(v): return P(compose(v.r.l,v.l),v.r.r) #o
def op_quote(v): 
  vq = v.l
  aff = not copyable(vq,copyDropTestDepth)
  rel = not droppable(vq,copyDropTestDepth)
  b = B(aff,rel,[Lit(vq)])
  return P(b,v.r)
def op_rel(v): 
  b0 = v.l
  bf = B(b0.affine, True, b0.code)
  return P(bf,v.r)
def op_aff(v): 
  b0 = v.l
  bf = B(True, b0.relevant, b0.code)
  return P(bf,v.r)

def op_distrib(v): 
  s = v.r.l
  if isL(s): return P(L(P(v.l,s.inL)),v.r.r)
  else:      return P(R(P(v.l,s.inR)),v.r.r)
  
def op_factor(v):
  if isL(v.l): 
    p = v.l.inL
    return P(L(p.l),P(L(p.r),v.r))
  else:
    p = v.l.inR
    return P(R(p.l),P(R(p.r),v.r))

def op_merge(v):
  if isL(v.l): return P(v.l.inL,v.r)
  else:        return P(v.l.inR,v.r)
def op_assert(v):
  if isL(v.l): raise error('ABC assertion failure @ ' + str(v))
  else: return P(v.l.inR,v.r) 
  
def op_gt(v): 
  if gt(v.r.l, v.l): return P(R(P(v.l,v.r.l)),v.r.r)
  else:              return P(L(P(v.r.l,v.l)),v.r.r)

def gt(x,y):
  while True: # to emulate tail recursion
    if isNumber(x): assertNumber(y); return (x > y)
    elif isUnit(x): assertUnit(y); return False
    elif isProd(x): 
      assert isProd(y), 'failed to compare product with ' + type(y)
      if gt(x.l,y.l): return True
      elif gt(y.l,x.l): return False
      x = x.r
      y = y.r
      continue # tail recursive
    elif isL(x):
      if isR(y): return False
      assert isL(y), 'failed to compare sum with ' + type(y)
      x = x.inL 
      y = y.inL
      continue # tail recursive
    elif isR(x):
      if isL(y): return True
      assert isR(y), 'failed to compare sum with ' + type(y)
      x = x.inR
      y = y.inR
      continue # tail recursive
    else: raise error('invalid type in comparison: ' + type(x))

def op_add(v): return P(v.l + v.r.l, v.r.r)
def op_negate(v): return P(-v.l, v.r)
def op_mul(v): return P(v.l * v.r.l, v.r.r)
def op_invert(v): return P(1/v.l, v.r)
def op_divmod(v): 
  quot,rem = divmod(v.r.l,v.l)
  return P(rem,P(quot,v.r.r))

def op_introNum(v): return P(number(0),v)
def op_digit(v,n): return P(10*v.l + number(n), v.r) 

# show a value up to depth N 
#  text will show in full
def showVal(v,N):
  if isNumber(v): return str(v)
  elif isUnit(v): return 'unit'
  elif isL(v) and isUnit(v.inL): return 'false'
  elif isR(v) and isUnit(v.inR): return 'true'
  elif (N < 1): return '...'
  elif isProd(v): return '(' + showVal(v.l,N-1) + '*' + showVal(v.r,N-1) + ')'
  elif isL(v): return '('+showVal(v.inL,N-1)+'+_)'
  elif isR(v):
    txt = tryValToText(v)
    if txt is None: return '(_+'+showVal(v.inR,N-1)+')'
    else: return showText(txt)
  elif isBlock(v): return '[code]'
  elif isSealedVal(v): return v.s + ':' + showVal(v.v,N)
  else: return '?(' + type(v) + ')?'

def showText(txt):
  if ('\n' in txt) or ('"' in txt): 
    return showMultiLineText(txt)
  else: return '"' + txt + '"' # AO inline text

def showMultiLineText(txt): 
  return '"' + '\n '.join(txt.splitlines()) + '\n~'
 

  

