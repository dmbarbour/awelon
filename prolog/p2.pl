%
% I've developed an alternative model for blocks to simplify
% inlining.
%
%    $ :: (forall y. Block((x*y)->(x'*y)) * (x * e) -> (x' * e)
%
% I'd also prefer to avoid use of intro1/elim1 for basic data 
% plumbing.
%
% So far my favorite set of primitives keeps `e` immobile, but 
% this design doesn't generalize well to also work for sum types.
% Having `e` frozen on the RHS simply doesn't make sense. Perhaps
% it would be best to enable its motion.
% 

prim([A,[B,C]], l, [[A,B],C]).
prim([[A,B],C], r, [A,[B,C]]).
prim([A,[B,C]], w, [B,[A,C]]).
prim([A,[B,[C,D]]], z, [A,[C,[B,D]]]).
prim(A,v,[A,unit]).
prim([A,unit],c,A).

%lib(A,id,A,[]).

%lib([A,[B,[C,D]]], wzw, [C,[B,[A,D]]], [w,z,w]). % rotx
%lib([[A,B],C], rwl, [[B,A],C], [r,w,l]). % swapE
%lib([[A,B],[C,D]], rzl, [[A,C], [B,D]], [r,z,l]). % zip2
%lib([A,[B,[C,[D,E]]]], lzr, [A,[B,[D,[C,E]]]], [l,z,r]).
%lib([[A,[B,C]],E], rwrzwll, [[[A,B],C],E], [r,w,r,z,w,l,l]).
%lib([[[A,B],C],E], rrwzlwl, [[A,[B,C]],E], [r,r,w,z,l,w,l]).
%lib([[[A,B],S],E], rrwll, [[[B,A],S],E], [r,r,w,l,l]).

%lib(A, intro1, [unit,A], [v,vrwlc]). % intro1
%lib([unit,A], elim1, A, [vrwlc,c]). % elim1

wrapE_post(N) :- path([[pb,[sn,v]],unit], 
                     [[v,unit],[unit,[pb,[[sn,unit],unit]]]], N).

unwrapE_pre(N) :- path([[v,unit],[unit,[pb,[[sn,unit],unit]]]],
                       [[pb,[sn,v]],unit], N).

dip_pre(N) :- path([[f,[x,s]],e],[f,[[s,e],[x,unit]]], N).
dip_post(N) :- path([[s,e],[x,unit]],[[x,s],e], N).

applyS_pre(N) :- path([[f,s],e], [f, [ [s,[unit,unit]], [e,unit] ]], N).
applyS_post(N) :- path( [[s,[unit,unit]], [e,unit]], [s,e], N). 

apply2_pre(N) :- 
    path( [ [f,[a,[b,s]]], e],
          [f, [ [[a,[b,unit]], [unit,unit]] , [[s,e],unit]]], N).
apply2_post(N) :- path( 
   [[ [a, [b, unit]], [unit,unit]] , [[s,e],unit]],
   [[a,[b,s]],e], N).

swapsn(N) :- path([[sn1,s],[h,[p,[[sn0,rns],e]]]],
                  [[sn0,s],[h,[p,[[sn1,rns],e]]]], N).
swapH(N) :- path([[h1,s],[h0,e]], [[h0,s],[h1,e]], N).

swapPower(N) :- path([[p1,s],[h,[p0,e]]], [[p0,s],[h,[p1,e]]], N).

ioapp_postStep(N) :- path([[sn,pb],unit], [unit,[unit,[pb,[[sn,unit],unit]]]], N).
iomsg_preStep(N) :- path([s,[h,[pb,e]]], [[pb,s],[h,e]], N).

introE_postStep(N) :- path([[sn,s],e], [[  [unit,[[sn,unit],unit]],  s],e], N). 
elimE_preStep(N) :- path([[  [unit,[[sn,unit],unit]],  s],e], [[sn,s],e], N).
swapE_step(N) :- 
    path([ [[s2,e2],s1], [h,[p,e1]]  ],
         [ [[s1,e1],s2], [h,[p,e2]]  ], N).

dip_preStep(N) :- path([[x,[b,s]],e], [b,[[s,e],[x,unit]]], N).
dip_postStep(N) :- path([[s,e],[x,unit]], [[x,s],e], N).

apply_preStep(N) :- path([[b,s],[h,e]],  [b, [[s,[unit,e]], [h,unit]]], N).
apply_postStep(N) :- path([[s,[unit,e]],[h,unit]], [s,[h,e]], N).

apply_adv_preStep(N) :- 
    path([[adv,[f,s]],e],
         [adv, [ [[f,unit],[unit,unit]], [[s,e],unit] ]], N).
apply_adv_postStep(N) :-
    path([ [[f,unit],[unit,unit]], [[s,e],unit] ],
           [f, [[s,e],unit]], N).

apply_0_prestep(N) :-
    path( [[f,s],e] , 
          [f, [[unit,[unit,unit]], [[s,e],unit]]], N). 

ap_postStep(N) :-
    path([ [[y,unit],[unit,unit]], [[s,e],unit] ],
           [[y,s],e], N).


unquote_preStep(N) :-
    path([[f,s],e], 
         [f, [[unit,unit], [[s,e],unit]]], N).
unquote_postStep(N) :-
    path([[[x,unit],unit], [[s,e],unit]],
         [[x,s],e], N).

pushE(N) :-
    path([[ [sE,eE],[x,sC] ],eC]
        ,[[ [[x,sE],eE], sC], eC], N).
popE(N) :-
    path([[ [[x,sE],eE], sC], eC]
        ,[[ [sE,eE],[x,sC] ],eC], N).

inline_preStep(N) :- path([[b,s],e], [b, [[s,e],unit]], N).

apply(N) :- path([[b,s],[h,e]], [b, [ [s,[unit,e]], h]], N).
endApply(N) :- path([ [s,[unit,e]], h], [s,[h,e]], N).

l_E(N) :- path([ [a,[b,c]], e],
               [ [[a,b],c], e], N).
r_E(N) :- path([ [[a,b],c], e], 
               [ [a,[b,c]], e], N).
z_E(N) :- path([ [a,[b,[c,d]]], e], 
               [ [a,[c,[b,d]]], e], N).
w_E(N) :- path([ [a,[b,c]], e],
               [ [b,[a,c]], e], N).
v_E(N) :- path([a,e], [[a,unit],e], N).
c_E(N) :- path([[a,unit],e], [a,e], N).


apply_wrap(N) :- path(a,[[a,unit],[unit,unit]],N).

assocr(N) :- path([[x,y],z], [x,[y,z]], N).
assocl(N) :- path([x,[y,z]], [[x,y],z], N).
rotx(N) :- path([a,[b,[c,d]]], [c,[b,[a,d]]],N).
rot2(N) :- path([x,[y,z]], [y,[x,z]], N).
swapE(N)   :- path([[x,y],e], [[y,x],e], N).
assoclE(N)  :- path([[x,[y,z]],e], [[[x,y],z],e], N).
assocrE(N)  :- path([[[x,y],z],e], [[x,[y,z]],e], N).

roxE(N)    :- path([[a,[b,[c,z]]],e], [[c,[b,[a,z]]],e], N).
rot3E(N)   :- path([[a,[b,[c,z]]],e], [[c,[a,[b,z]]],e], N).
rot2E(N)   :- path([[x,[y,z]],e], [[y,[x,z]],e], N).
zip2(N)     :- path([[a,b],[c,e]], [[a,c],[b,e]], N).
rot3(N)     :- path([a,[b,[c,d]]], [c,[a,[b,d]]], N).
rot3_(N)    :- path([c,[a,[b,d]]], [a,[b,[c,d]]], N).
rot4(N)     :- path([a,[b,[c,[d,e]]]], [d,[a,[b,[c,e]]]], N).
rot5(N)     :- path([a,[b,[c,[d,[e,f]]]]], [e,[a,[b,[c,[d,f]]]]], N).
zip2E(N)   :- path([[[a,b],[c,d]],e], [[[a,c],[b,d]],e], N).

take(N) :- path([[x,s],[h,e]], [s,[[x,h],e]], N).
put(N) :- path([s,[[x,h],e]], [[x,s],[h,e]], N).
jugl2(N) :- path([s,[[x,[y,h]],e]], [s,[[y,[x,h]],e]], N).

t0(N) :- path([[x,s],e], [s,[x,e]], N).
p0(N) :- path([s,[x,e]], [[x,s],e], N).


l_onStack(N) :- 
    path([[ [x,[y,z]], s],e], 
         [[ [[x,y],z], s],e], N).
r_onStack(N) :- 
    path([[ [[x,y],z], s],e], 
         [[ [x,[y,z]], s],e], N).
z_onStack(N) :-
    path([[ [a,[b,[c,d]]], s],e],
         [[ [a,[c,[b,d]]], s],e], N).

pzip_onStack(N) :-
    path([[ [[a,b],[c,d]], s],e],
         [[ [[a,c],[b,d]], s],e], N).


%lib([[A,B],C], swapE, [[B,A],C], [r,w,l]).
%lib(
%lib([A,[B,[C,[D,E]]]], r4, [D,[A,[B,[C,E]]]], [l,w,c,w,r,w,c]).
%lib([A,[B,[C,[D,[E,F]]]]], r5, [E,[A,[B,[C,[D,F]]]]], [l,r4,w,r,w,c]).
%lib([[X,Y],ENV],swap,[[Y,X],ENV], [r,w,l]).
%lib([[X,[Y,Z]],ENV],assocl,[[[X,Y],Z],ENV],[r,w,r,w,c,l,l]).
%lib([[[X,Y],Z],ENV],assocr,[[X,[Y,Z]],ENV],[r,r,c,w,l,w,l]).
%lib([[A,[B,[C,Z]]],ENV],rox,[[C,[B,[A,Z]]],ENV], [r,w,r,c,w,r,c,l,w,c,l,w,l]).
%lib([[A,[B,[C,Z]]],ENV],rot3,[[C,[A,[B,Z]]],ENV],[r,w,r,w,r,c,l,w,c,l,w,l]).
%lib([[A,[B,Z]],ENV],rot2,[[B,[A,Z]],ENV],[r,w,r,c,l,w,l]).
%lib([[[A,B],[C,D]],ENV], zip2, [[[A,C],[B,D]],ENV], [assocr, rot3, rot2, assocl]).


% Proof of generality is ability to build zipper operations
% on the first element of a structure.

% Zipper Operation...
%     zwrap :: x <~> x*(1*1)  :: zunwrap_
%     zf    :: (x*y)*(l*r) <~> x*(1*(y*(l*r))) :: zuf
%     zs    :: (x*y)*(l*r) <~> y*((x*(l*r))     :: zus
%     (representation subject to change) 
%     zswap - switch target of zipper (2nd) with object on stack (1st)

zwrap(N) :- 
    onStack(S0,x),
    onStack(SF,[x,[unit,unit]]),!,
    path(S0,SF,N).
% intro1 intro1 l w l
zunwrap_(N) :- 
    onStack(S0,x),
    onStack(SF,[x,[unit,unit]]),!,
    path(SF,S0,N).
% r w r elim1 elim1


zf(N) :-
    onStack(S0, [[x,y],[l,r]]),
    onStack(SF, [x,[unit,[y,[l,r]]]]),!,
    path(S0,SF,N).
% assocr intro1 l rot2



zuf(N) :-
    onStack(S0, [[x,y],[l,r]]),
    onStack(SF, [x,[unit,[y,[l,r]]]]),!,
    path(SF,S0,N).
% rot2 r elim1 assocl

zs(N) :-
    onStack(S0, [[x,y],[l,r]]),
    onStack(SF, [y,[[x,l],r]]),!,
    path(S0,SF,N).
% zip2 rot2

zus(N) :-
    onStack(S0, [[x,y],[l,r]]),
    onStack(SF, [y,[[x,l],r]]),!,
    path(SF,S0,N).
% rot2 zip2

zswap(N) :-
    path([x,[[z,w],e]],
         [z,[[x,w],e]], N).
% l rot2 r

onStack([X,opaqueEnv],X).

% closed (cyclic) paths, i.e. find identity functions
cpath(X,N) :- uPath(X,X,N,[],P),length(P,NL),write(P),write(NL).

env8([[[a,b],[c,d]], [[e,f],[g,h]]]).

env16([[[[a,b],[c,d]], [[e,f],[g,h]]],[[[i,j],[k,l]], [[m,n],[o,p]]]]).

env32( 
  [[[[[a,b],[c,d]], [[e,f],[g,h]]],[[[i,j],[k,l]], [[m,n],[o,p]]]],
   [[[[q,r],[s,t]], [[u,v],[w,x]]],[[[y,z],[2,3]], [[4,5],[6,7]]]]] ).

cpath8(N) :- env8(E),!,cpath(E,N).
cpath16(N) :- env16(E),!,cpath(E,N).
cpath32(N) :- env32(E),!,cpath(E,N).




% test for a function of up to N elements.
path(X,Y,N) :- uPath(X,Y,N,[X],P),length(P,NL),write(P),write(NL).
% add primitive
uPath(X,Y,N,H,[OP|P]) :- (N > 0),prim(X,OP,S),not(member(S,H)),uPath(S,Y,(N-1),[S|H],P).
% add library function
%uPath(X,Y,N,H,[W|P]) :- (N > 0),lib(X,W,S,_),not(member(S,H)),uPath(S,Y,(N-1),[S|H],P).
% function found!
uPath(X,X,N,_,[]) :- (N >= 0).

% validate the library functions.
testLib(W,X) :-
    lib(X,W,Z,P),!,
    write('validating '), write(W), 
    write(' :: '), write(X), write(' -> '), writeln(Z),
    smallSteps(X,Z,P).

testPath(P,X) :- 
    write('\t\t'),writeln(X),
    smallSteps(X,_,P).

smallSteps(X,X,[]) :- writeln('DONE!').
smallSteps(X,Z,[W|P]) :- 
    prim(X,W,Y),
    write(W), write('\t\t'),writeln(Y),
    smallSteps(Y,Z,P).
%smallSteps(X,Z,[W|P]) :-
%    lib(X,W,Y,DEF),
%    write('#'),write(W),writeln('{'),
%    smallSteps(X,Y,DEF),!,
%    write('}#'),writeln(W),
%    smallSteps(Y,Z,P).



