
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
#   Lit - literals contain a value type (e→(L*e))
#
# Todo: Find an alternative model for code and values to respect RPython's
#  constraint on variables having a single type. 
#
# Code and values should not bind to the Python environment, at least
# not strongly. E.g. blocks can be serialized to ABC and recovered.
#
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
def divQ(n1,n2): return divmod(n1,n2)  # n1/n2 → (q,r)
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
  if isL(v): 
     assertUnit(v.inL); 
     return u''
  else: 
     p = v.inR
     c = unichr(int(p.l))
     s = valToText(p.r)
     return (c + s)

def copyable(x):
  if isNumber(x) or isUnit(x): return True
  elif isProd(x): return (copyable(x.l) and copyable(x.r))
  elif isL(x): return copyable(x.inL)
  elif isR(x): return copyable(x.inR)
  elif isBlock(x): return (not x.affine)
  elif isSealedVal(x): return copyable(x.v)
  else: raise error("invalid value %r passed to 'copyable'" % (x,))

def droppable(x):
  if isNumber(x) or isUnit(x): return True
  elif isProd(x): return (droppable(x.l) and droppable(x.r))
  elif isL(x): return droppable(x.inL)
  elif isR(x): return droppable(x.inR)
  elif isBlock(x): return (not x.relevant)
  elif isSealedVal(x): return droppable(x.v)
  else: raise error("invalid value %r passed to 'droppable'" % (x,))


# Blocks are very simple in pywelon:
#   affine and relevant are boolean
#   code is a list of int/Lit/Inv values
#     this list should be considered immutable
#     (pypy doesn't support variable-length tuples)
B = namedtuple('B', ('affine','relevant','code'))

def compose(b1,b2):
  aff = b1.affine or b2.affine
  rel = b1.relevant or b2.relevant
  code = composeCode(b1.code,b2.code)
  return B(aff,rel,code)
def composeCode(c1,c2):
  return (c1 + c2) # no simplification for now


# Code types for ABC operators
Lit = namedtuple('Lit',('val',)) # literal value (e → (L*e))
Inv = namedtuple('Inv',('tok',)) # {token}

def isOp(x):  return type(x) is int
def isLit(x): return type(x) is Lit
def isInv(x): return type(x) is Inv
 



