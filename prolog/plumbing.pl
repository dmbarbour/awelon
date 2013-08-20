
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

% derived functions (without using blocks)
% these can be validated using testLib
lib([[A,B],C],  assocr  ,[A,[B,C]],
    [swap,assocl,swap,assocl,swap]).
lib([A,[B,C]],  rot2  ,[B,[A,C]],
    [intro1,rot3,intro1,rot3,elim1,elim1]).
lib([[A,B],[C,D]],  zip2  ,[[A,C],[B,D]],
    [assocr,rot3,rot2,assocl]).
lib([X,[[SL,[SC,SR]],H]],  insert  ,[[SL,[[X,SC],SR]],H],
    [rot2,swap,rot3,rot2,zip2,rot3,rot3,swap]).
lib([[SL,[[X,SC],SR]],H],  extract  ,[X,[[SL,[SC,SR]],H]],
    [swap,rot3,zip2,rot3,rot2,swap,rot2]).
lib([SC,[[SL,SR],H]],  insertStack  ,[[SL,[SC,SR]],H],
    [assocl,swap,rot3,rot2,swap]).
lib([[SL,[SC,SR]],H],  extractStack  ,[SC,[[SL,SR],H]],
    [swap,rot3,assocl,swap,rot2]).
lib([[SL,[[X,SC],SR]],[HL,HR]], take  ,[[SL,[SC,SR]],[HL,[X,HR]]],
    [extract,rot3,rot3]).
lib([[SL,[SC,SR]],[HL,[X,HR]]], put  ,[[SL,[[X,SC],SR]],[HL,HR]],
    [rot3,insert]).
lib([[SL,[SC,SR]],H],  newStack  ,[[SL, [SC, [unit,SR]]], H],
    [swap,assocl,intro1,rot3,rot3,assocr,swap]).
lib([[SL, [SC, [unit,SR]]], H],  remStack  ,[[SL,[SC,SR]],H],
    [swap,assocl,rot3,elim1,assocr,swap]).
lib([[[ST,SL],[SC,SR]],H],  stepLeft,  [[SL,[ST,[SC,SR]]],H],
    [assocr,zip2,rot2,assocl]).
lib([[SL,[SC,[ST,SR]]],H],  stepRight,  [[[SC,SL],[ST,SR]],H],
    [assocr,rot2,zip2,assocl]).

lib(X,  wrapV,  WX, 
    [intro1,swap,intro1,swap,intro1,intro1,intro1,assocl,swap])
    :- wrapped(X,WX).
lib(WX, unwrapV, X,
    [swap,assocr,elim1,elim1,elim1,swap,elim1,swap,elim1])
    :- wrapped(X,WX).

% hmmm. wrapVal is nice, but not exactly what I want. I need a 
%  form of `appX` that wraps the target object first, then
%  unwraps it afterwards.

   
% partial lib functions (i.e. prep for 'first')
% commented out because I don't like them showing up in paths
%lib([F,[X,Y]],  prep_second  ,[F,[Y,X]], [assocl,swap,rot2]).
%lib([[SL,[[F,SC],SR]],H],  prep_appS  ,[F,[SC,[[SL,SR],H]]],
%    [extractStack,assocr]).
%lib([[SL,[[F,[X,SC]],SR]],H], prep_appX  ,[F,[X,[[SL,[SC,SR]],H]]],
%    [extract,rot2,extract,rot3]).
%lib([[SL,[[F,SC],SR]],H],  prep_appE  ,[F, [[[SL,[SC,SR]],H],unit]  ],
%    [extract,intro1,swap,assocr]). % unit is there so we can apply 'first'

newStack(N) :- path([[sL,[sC,sR]],h], 
                    [[sL,[sC,[unit,sR]]],h], N).
remStack(N) :- path([[sL,[sC,[unit,sR]]],h],
                    [[sL,[sC,sR]],h], N).

stepLeft(N) :- path([[[sT,sL],[sC,sR]],h],
                    [[sL,[sT,[sC,sR]]],h], N).
stepRight(N) :- path( [[sL,[sC,[sT,sR]]],h],
                      [[[sC,sL],[sT,sR]],h], N).  

% create a new 
wrapped(X,[[unit,[[X,unit],unit]],[unit,unit]]).  

wrapEnv(N) :- wrapped(x,WX),!,path(x,WX,N).
unwrapEnv(N) :- wrapped(x,WX),!,path(WX,x,N).


% test for a function of up to N elements.
path(X,Y,N) :- uPath(X,Y,N,[X],P),length(P,NL)
              ,write(P),write(NL).
% add primitive
uPath(X,Y,N,H,[OP|P]) :- (N > 0),prim(X,OP,S),
	not(member(S,H)),uPath(S,Y,(N-1),[S|H],P).
% add library function
uPath(X,Y,N,H,[W|P]) :-
	(N > 0),lib(X,W,S,_),
	not(member(S,H)),uPath(S,Y,(N-1),[S|H],P).
% function found!
uPath(X,X,N,_,[]) :- (N >= 0).

% validate the library functions.
testLib(W,X) :-
    lib(X,W,Z,P),!,
    write('validating '), write(W), 
    write(' :: '), write(X), write(' -> '), writeln(Z),
    smallSteps(X,Z,P).

smallSteps(X,X,[]) :- writeln('DONE!').
smallSteps(X,Z,[W|P]) :- 
    prim(X,W,Y),!,
    write(W), write('\t\t'),writeln(Y),
    smallSteps(Y,Z,P).
smallSteps(X,Z,[W|P]) :-
    lib(X,W,Y,DEF),!,
    write('#'),write(W),writeln('{'),
    smallSteps(X,Y,DEF),!,
    write('}#'),writeln(W),
    smallSteps(Y,Z,P).








    

