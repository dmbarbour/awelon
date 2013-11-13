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
prim([[A,B],[C,D]], z, [[A,C],[B,D]]).
prim(A,v,[A,unit]).
prim([A,unit],c,A).

% FAVORITE PRIMITIVE SET SO FAR
%prim([A,[B,C]], l, [[A,B],C]).
%prim([[A,B],C], r, [A,[B,C]]).
%prim([A,[B,C]], w, [B,[A,C]]).
%prim([A,[B,[C,D]]], x, [C,[B,[A,D]]]).
%prim(ENV,intro1,[unit,ENV]).
%prim([unit,ENV],elim1,ENV).

assocr(N) :- path([[x,y],z], [x,[y,z]], N).
assocl(N) :- path([x,[y,z]], [[x,y],z], N).
rotx(N) :- path([a,[b,[c,d]]], [c,[b,[a,d]]],N).
rot2(N) :- path([x,[y,z]], [y,[x,z]], N).
swapE(N)   :- path([[x,y],e], [[y,x],e], N).
assoclE(N) :- path([[x,[y,z]],e], [[[x,y],z],e], N).
assocrE(N) :- path([[[x,y],z],e], [[x,[y,z]],e], N).
roxE(N)    :- path([[a,[b,[c,z]]],e], [[c,[b,[a,z]]],e], N).
rot3E(N)   :- path([[a,[b,[c,z]]],e], [[c,[a,[b,z]]],e], N).
rot2E(N)   :- path([[x,[y,z]],e], [[y,[x,z]],e], N).
zip2(N)     :- path([[a,b],[c,e]], [[a,c],[b,e]], N).
rot3(N)     :- path([a,[b,[c,d]]], [c,[a,[b,d]]], N).
rot3_(N)    :- path([c,[a,[b,d]]], [a,[b,[c,d]]], N).
rot4(N)     :- path([a,[b,[c,[d,e]]]], [d,[a,[b,[c,e]]]], N).
rot5(N)     :- path([a,[b,[c,[d,[e,f]]]]], [e,[a,[b,[c,[d,f]]]]], N).
zip2E(N)   :- path([[[a,b],[c,d]],e], [[[a,c],[b,d]],e], N).

lib(A,id,A,[]).
%lib([A,[B,[C,D]]], rotx, [C,[B,[A,D]]], [w,l,z,r,w]).
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




% test for a function of up to N elements.
path(X,Y,N) :- uPath(X,Y,N,[X],P),length(P,NL),write(P),write(NL).
% add primitive
uPath(X,Y,N,H,[OP|P]) :- (N > 0),prim(X,OP,S),not(member(S,H)),uPath(S,Y,(N-1),[S|H],P).
% add library function
uPath(X,Y,N,H,[W|P]) :- (N > 0),lib(X,W,S,_),not(member(S,H)),uPath(S,Y,(N-1),[S|H],P).
% function found!
uPath(X,X,N,_,[]) :- (N >= 0).

% validate the library functions.
testLib(W,X) :-
    lib(X,W,Z,P),!,
    write('validating '), write(W), 
    write(' :: '), write(X), write(' -> '), writeln(Z),
    smallSteps(X,Z,P).

testPath(P,X) :-  smallSteps(X,_,P).

smallSteps(X,X,[]) :- writeln('DONE!').
smallSteps(X,Z,[W|P]) :- 
    prim(X,W,Y),
    write(W), write('\t\t'),writeln(Y),
    smallSteps(Y,Z,P).
smallSteps(X,Z,[W|P]) :-
    lib(X,W,Y,DEF),
    write('#'),write(W),writeln('{'),
    smallSteps(X,Y,DEF),!,
    write('}#'),writeln(W),
    smallSteps(Y,Z,P).



