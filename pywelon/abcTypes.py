
# Types for Awelon Bytecode (ABC)
#
# Value Types
#   U - empty class acts as value
#   P - named tuple for products
#   L,R - named tuples for sums
#   float - current implementation of numbers (might switch to fractions)
#   numbers are Python floats (might switch to fractions)
#   blocks use a dedicated class?? or maybe a tuple of Code?
#   sealed values use named tuple SV
#
# Code Types:
#   Ops - primitives contain a string of operators
#   Inv - invocations contain a string {foo}
#     Seal   - specialized invocation {:sealer}
#     Unseal - specialized invocation {.sealer}
#     Anno   - specialized invocation {&anno}
#   Lit - literals contain a value type (e→(L*e))
#     permits arbitrary literals, though ABC only
#     directly supports texts and blocks. Pywelon
#     will use literals for numbers and quotations.
#
# Code and values should not bind to the Python environment, at least
# not strongly. E.g. blocks can be serialized to ABC and recovered.
#
# Todo: validate that this is all cool for PyPy's RPython
#

from collections import namedtuple

# ABC types in Python:
P = namedtuple('P','l r',verbose=False,rename=False)
def isProd(x): return (type(x) is P)

L = namedtuple('L','inL',verbose=False,rename=False)
R = namedtuple('R','inR',verbose=False,rename=False)
def isL(x): return (type(x) is L)
def isR(x): return (type(x) is R)
def isSum(x): return (isL(x) or isR(x))
def assertSum(x): assert isSum(x), "sum expected; received %r" % (x,)

class U(tuple):
  def __init__(self): 
    assert False, "unit is the class; do not instantiate"
unit = U
def isUnit(x): return (x is U)
def assertUnit(x): assert isUnit(x), "unit expected; received %r" % (x,)

SV = namedtuple('SV','s v', verbose=False, rename=False)
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
def textToVal(s): 
  t = L(unit)
  for c in reversed(s):
    t = R(P(number(ord(c)),t))
  return t


def copyable(x):
  if isNumber(x) or isUnit(x): return True
  elif isProd(x): return (copyable(x.l) and copyable(x.r))
  elif isL(x): return copyable(x.inL)
  elif isR(x): return copyable(x.inR)
  elif isBlock(x): return (not x.affine)
  elif isSealedVal(x): return copyable(x.v)
  else: assert False, "invalid value %r passed to 'copyable'" % (x,)

def droppable(x):
  if isNumber(x) or isUnit(x): return True
  elif isProd(x): return (droppable(x.l) and droppable(x.r))
  elif isL(x): return droppable(x.inL)
  elif isR(x): return droppable(x.inR)
  elif isBlock(x): return (not x.relevant)
  elif isSealedVal(x): return droppable(x.v)
  else: assert False, "invalid value %r passed to 'droppable'" % (x,)

# join two operation strings with limited simplification
#  (I'm not sure this is worthwhile. But implementing it was cathartic.)
def joinOpStrings(l,r):
  if (not l): return r
  elif (not r): return l
  elif (opsCancel(ord(l[-1]),ord(r[0]))): return joinOpStrings(l[:-1],r[1:])
  else: return (l + r)
def opsCancel(op1,op2): 
  return ((op1 == op1) and selfCancel(op1)) \
      or asymOpsCancel(op1,op2) \
      or asymOpsCancel(op2,op1)
def selfCancel(op):  # ww, zz, WW, ZZ
  return (op == 119) or (op == 122) \
      or (op == 87)  or (op == 90)
def asymOpsCancel(op1,op2): # lr, cv, LR, CV
  return ((op1 == 108) and (op2 == 114)) \
      or ((op1 == 99)  and (op2 == 118)) \
      or ((op1 == 76)  and (op2 == 82))  \
      or ((op1 == 67)  and (op2 == 86))
  



