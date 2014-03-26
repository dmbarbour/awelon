
import sys
import io
from .abcTypes import *
from .parseABC import *
from .effects import *
    
from collections import namedtuple

# runABC takes a source text (for the program) and an
# 'environment' (an initial value). It returns the
# final environment value, after processing the
# entire source.
#
# To consider:
#   shift this into a class or object
#   such that the env & invoker is part of the object
def runABC(source, env, invoker): 
  return loopABC(parseABC(source),0,env,invoker)

# reify stack for runABC
Stack = namedtuple('Stack',['code','pc','context','next'])

def loopABC(code,pc,env,invoker):
  stack = None # affected by '$' and '?' ops
  while True:
    while(pc < len(code)):
      if(isOp(code[pc])):
        op = code[pc]
        if(119 == op):   env = op_w(env) #w
        elif(108 == op): env = op_l(env) #l
        elif(114 == op): env = op_r(env) #r
        elif(122 == op): env = op_z(env) #z
        elif(118 == op): env = op_v(env) #v
        elif(99 == op):  env = op_c(env) #c
        elif(36 == op): #$
          if ((pc+2) == len(code)) and (99 == code[pc+1]):  #$c]
            assertUnit(env.r.r) # tail-call; no new stack frame
          else: stack = Stack(code,pc,env.r.r,stack)
          code = env.l.code
          env  = env.r.l
          pc   = 0
          continue # to avoid incrementing pc
        elif(37 == op):  env = op_drop(env) #%
        elif(94 == op):  env = op_copy(env) #^
        elif(111 == op): env = op_comp(env) #o
        elif(39 == op):  env = op_quote(env) #'
        elif(107 == op): env = op_rel(env) #k
        elif(102 == op): env = op_aff(env) #f
        elif(87 == op):  env = op_W(env) #W
        elif(76 == op):  env = op_L(env) #L
        elif(82 == op):  env = op_R(env) #R
        elif(90 == op):  env = op_Z(env) #Z
        elif(86 == op):  env = op_V(env) #V
        elif(67 == op):  env = op_C(env) #C
        elif(68 == op):  env = op_distrib(env) #D
        elif(70 == op):  env = op_factor(env) #F
        elif(77 == op):  env = op_merge(env) #M
        elif(75 == op):  env = op_assert(env) #K
        elif(62 == op):  env = op_gt(env) # >
        elif(63 == op): # ?
          assert (not env.l.relevant)
          if(isR(env.r.l)): env = env.r # drop due to condition
          else: 
            stack = Stack(code,pc,env.r.r,stack)
            code = env.l.code
            env = env.r.l.inL
            pc = 0
            continue # to avoid incrementing pc
        elif(43 == op):  env = op_add(env) #+ 
        elif(45 == op):  env = op_negate(env) #-
        elif(42 == op):  env = op_mul(env) #*
        elif(47 == op):  env = op_invert(env) #/
        elif(81 == op):  env = op_divmod(env) #Q
        elif(35 == op):  env = op_introNum(env) ##
        elif((48 <= op) and (op <= 57)): env = op_digit(env,op-48) #0..9
        elif((10 == op) or (32 == op)): pass # identity (LF or SP)
        else: raise error('unrecognized op code: ' + op)
      elif(isInv(code[pc])):
        tok = code[pc].tok
        if not tok:           env = invoker.invoke(tok,env)
        elif (':' == tok[0]): env = SV(tok[1:],env)
        elif ('.' == tok[0]): env = unseal(tok[1:],env)
        elif ('&' == tok[0]): env = invoker.anno(tok[1:],env)
        else:                 env = invoker.invoke(tok,env)
      elif(isLit(code[pc])):  env = P(code[pc].val, env)
      else: raise error('unrecognized code: ' + code[pc])

      pc += 1 # continue to next operation
      # repeat while code is available
    # if no code is available for the current call, try the stack
    if stack is None: # stack is clear
       return env # successful exit from loopABC
    else: # pop stack and continue
       code = stack.code
       pc = stack.pc # should still point to the calling operation
       if (36 == code[pc]): # return from '$' call
         env = P(env,stack.context)
       elif (63 == code[pc]): # return from '?' call
         env = P(L(env),stack.context) 
       else: raise error('unrecognized stack operator: ' + code[pc])
       stack = stack.next
       pc += 1 # continue to next operation
    # repeat forever

def printEnvSummary(env): 
  # currently, just print top object on stack
  if isProd(env) and isProd(env.l): 
    sys.stdout.write(showVal(env.l.l,16) + u'\n')

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

def defaultInvoker(): return DefaultInvoker()
def defaultEnv(): return aoStdEnv()
def defaultProgram(): return sys.stdin.read()

def main(): 
  finalEnv = runABC(defaultProgram(), defaultEnv(), defaultInvoker())
  printEnvSummary(finalEnv)

# Python's semantic hack for modules as programs.
if __name__ == "__main__": main()
