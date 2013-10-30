
% this module helps find "blockless" data plumbing ops.
% blockless ops are useful for bootstrap, and also seem
% just a bit elegant and cleaner.
%
% the decision was to support 'rot3' to make these possible.

% primitives for pure data plumbing (except id)
prim([X,[Y,Z]],assocl,[[X,Y],Z]).
prim(X,intro1,[unit,X]).
prim([unit,X],elim1,X).
prim([A,B],swap,[B,A]).
prim([A,[B,[C,D]]],rot3,[C,[A,[B,D]]]).

%prim((X,(Y,Z)), assocls, ((X,Y),Z)).
%prim(X,intro0,(zero,X)).
%prim((zero,X),elim0,X).
%prim((A,B),mirror,(B,A)).
%prim((A,(B,(C,D))),rot3s,(C,(A,(B,D)))).


% derived functions (without using blocks)
% these can be validated using testLib

lib([[A,B],C],  assocr  ,[A,[B,C]], [swap,assocl,swap,assocl,swap]).
lib([A,[B,C]],  rot2  ,[B,[A,C]],   [intro1,rot3,intro1,rot3,elim1,elim1]).
lib([[A,B],[C,D]],  zip2  ,[[A,C],[B,D]], [assocr,rot3,rot2,assocl]).
lib([A,[B,[C,[D,E]]]],  rot4,  [D,[A,[B,[C,E]]]], [assocl,rot3,rot2,assocr,rot3]).
lib([A,[B,[C,[D,[E,F]]]]],  rot5,  [E,[A,[B,[C,[D,F]]]]], [assocl,rot4,rot2,assocr,rot3]).
% lib([[A,[B,C]],[D,[E,F]]],  zip3  ,[[A,D],[[B,E],[C,F]]], [zip2,rot3,rot3,zip2,rot3]).

%lib([A,[B,[C,[D,[E,[F,G]]]]]],  rot6,  [F,[A,[B,[C,[D,[E,G]]]]]],
%    [assocl,rot5,rot2,assocr,rot3]).

%lib([C,[A,[B,D]]], urot3,  [A,[B,[C,D]]], [rot3,rot3]).
%lib([D,[A,[B,[C,E]]]],  urot4,  [A,[B,[C,[D,E]]]],
%    [urot3,assocl,rot2,urot3,assocr]).
%lib([E,[A,[B,[C,[D,F]]]]],  urot5,  [A,[B,[C,[D,[E,F]]]]],
%    [urot3,assocl,rot2,urot4,assocr]).
%lib([F,[A,[B,[C,[D,[E,G]]]]]],  urot6,  [A,[B,[C,[D,[E,[F,G]]]]]],
%    [urot3,assocl,rot2,urot5,assocr]).


%lib([[A,[B,[C,D]]],[E,[F,[G,H]]]], zip4, [[A,E],[[B,F],[[C,G],[D,H]]]], 
%    [zip3,assocl,zip2,zip3,assocr]).
%lib([[A,D],[[B,E],[C,F]]],  uzip3, [[A,[B,C]],[D,[E,F]]], 
%    [urot3,zip2,rot3,zip2]).
%lib([[A,E],[[B,F],[[C,G],[D,H]]]], uzip4,  [[A,[B,[C,D]]],[E,[F,[G,H]]]],
%    [assocl,uzip3,zip2,assocr,uzip3]).


%lib([[A,[B,S]],E],  roll2,  [[B,[A,S]],E], [swap,rot3,rot2,swap]).
%lib([[A,[B,[C,S]]],E],  roll3,  [[C,[A,[B,S]]],E], [swap,rot4,rot2,swap]).

%lib([[X,S],[H,E]], take, [S,[[X,H],E]], [zip2,rot2]).
%lib([S,[[X,H],E]], put, [[X,S],[H,E]], [rot2,zip2]).
%lib([S,[[X,[Y,H]],E]], juggle2, [S,[[Y,[X,H]],E]], 
%    [rot2,swap,rot3,rot2,swap,rot2]).
%lib([S,[[X,[Y,[Z,H]]],E]], juggle3, [S,[[Z,[X,[Y,H]]],E]],
%    [rot2,swap,rot4,rot2,swap,rot2]).

%lib([[[A,B],S],E],  xf  , [[A,[B,S]],E], [swap,rot2,assocr,rot3,swap]).
%lib([[[A,B],S],E],  xs  , [[B,[A,S]],E], [assocr,zip2,rot2,assocl]).
%lib([[A,[B,S]],E],  cf  , [[[A,B],S],E], [swap,rot3,rot3,assocl,rot2,swap]).
%lib([[B,[A,S]],E],  cs  , [[[A,B],S],E], [assocr,rot2,zip2,assocl]).


% Zipper Operation...
%     zwrap :: x <~> x*(1*1)  :: zunwrap_
%     zf    :: (x*y)*(l*r) <~> x*(1*((y*l)*r)) :: zuf
%     zs    :: (x*y)*(l*r) <~> y*((x*l)*r)     :: zus
%     (representation subject to change) 
%     zswap - switch target of zipper (2nd) with object on stack (1st)

zwrap(N) :- 
    onStack(S0,x),
    onStack(SF,[x,[unit,unit]]),!,
    path(S0,SF,N).
zunwrap_(N) :- 
    onStack(S0,x),
    onStack(SF,[x,[unit,unit]]),!,
    path(SF,S0,N).
zf(N) :-
    onStack(S0, [[x,y],[l,r]]),
    onStack(SF, [x,[unit,[y,[l,r]]]]),!,
    path(S0,SF,N).
zuf(N) :-
    onStack(S0, [[x,y],[l,r]]),
    onStack(SF, [x,[unit,[y,[l,r]]]]),!,
    path(SF,S0,N).
zs(N) :-
    onStack(S0, [[x,y],[l,r]]),
    onStack(SF, [y,[[x,l],r]]),!,
    path(S0,SF,N).
zus(N) :-
    onStack(S0, [[x,y],[l,r]]),
    onStack(SF, [y,[[x,l],r]]),!,
    path(SF,S0,N).
zswap(N) :-
    path([[x,[[z,w],s]],e],
         [[z,[[x,w],s]],e], N).
 

onStack([[P,s],e],P). % convenience function



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








    

