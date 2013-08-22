
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
%lib([[A,B],[C,D]],  foil,  [[A,D],[B,C]], 
%    [assocr,rot3,rot3,assocl,swap]).
%lib([A,[B,[C,[D,E]]]],  rot4,  [D,[A,[B,[C,E]]]],
%    [assocl,rot3,rot2,assocr,rot3]).
lib([A,[B,[C,[D,[E,F]]]]],  rot5,  [E,[A,[B,[C,[D,F]]]]],
    [assocl,rot4,rot2,assocr,rot3]).
lib([A,[B,[C,[D,[E,[F,G]]]]]],  rot6,  [F,[A,[B,[C,[D,[E,G]]]]]],
    [assocl,rot5,rot2,assocr,rot3]).
% some functions I'd prefer to avoid when building some functions
% comment them back in if you want to try them.
lib([X,[[SL,[SC,SR]],H]],  insert  ,[[SL,[[X,SC],SR]],H],
    [rot2,swap,rot3,rot2,zip2,rot3,rot3,swap]).
lib([[SL,[[X,SC],SR]],H],  extract  ,[X,[[SL,[SC,SR]],H]],
    [swap,rot3,zip2,rot3,rot2,swap,rot2]).
lib([[SL,[[X,SC],SR]],[HL,HR]], take  ,[[SL,[SC,SR]],[HL,[X,HR]]],
    [extract,rot3,rot3]).
lib([[SL,[SC,SR]],[HL,[X,HR]]], put  ,[[SL,[[X,SC],SR]],[HL,HR]],
    [rot3,insert]).
lib([S,[HL,[X,[Y,HR]]]],  jugl2  ,[S,[HL, [Y,[X,HR]]]],
    [rot4, rot3, rot3]).
lib([S,[HL,[X,[Y,[Z,HR]]]]],  jugl3  ,[S,[HL, [Z,[X,[Y,HR]]]]],
    [assocl,rot4,rot2,assocr]).
lib([S,[HL,[A,[B,[C,[D,HR]]]]]], jugl4  , [S,[HL, [D,[A,[B,[C,HR]]]]]],
    [assocl,jugl3,assocr,jugl2]).
lib([[SL,[[X,[Y,SC]],SR]],H], roll2, [[SL,[[Y,[X,SC]],SR]],H],
    [swap,rot3,swap,rot3,rot2,swap,rot3,rot3,swap]).
lib([[SL,[[X,[Y,[Z,SC]]],SR]],H], roll3, [[SL,[[Z,[X,[Y,SC]]],SR]],H],
    [take,roll2,put,roll2]).
lib([[SL,[[A,[B,[C,[D,SC]]]],SR]],H], roll4, [[SL,[[D,[A,[B,[C,SC]]]],SR]],H],
    [take,roll3,put,roll2]).

lib([[SL,[[[X,Y],SC],SR]],H],  expand,  [[SL,[[X,[Y,SC]],SR]],H],
    [swap,rot3,swap,rot2,assocr,rot3,swap,rot3,rot3,swap]).
lib([[SL,[[X,[Y,SC]],SR]],H],  shrink,  [[SL,[[[X,Y],SC],SR]],H],
    [swap,rot3,swap,rot3,rot3,assocl,rot2,swap,rot3,rot3,swap]).
lib([S,[HL,[[X,Y],HR]]],  expandH,  [S, [HL, [X, [Y, HR]]]],  % spread in hand
    [assocl,rot2,assocr,rot3,assocr]).


%lib([SC,[[SL,SR],H]],  insertStack  ,[[SL,[SC,SR]],H],
%    [assocl,swap,rot3,rot2,swap]).
%lib([[SL,[SC,SR]],H],  extractStack  ,[SC,[[SL,SR],H]],
%    [swap,rot3,assocl,swap,rot2]).
%lib([[SL,[SC,SR]],H],  newStack  ,[[SL, [SC, [unit,SR]]], H],
%    [swap,assocl,intro1,rot3,rot3,assocr,swap]).
%lib([[SL, [SC, [unit,SR]]], H],  remStack  ,[[SL,[SC,SR]],H],
%    [swap,assocl,rot3,elim1,assocr,swap]).
%lib([[[ST,SL],[SC,SR]],H],  stepLeft,  [[SL,[ST,[SC,SR]]],H],
%    [assocr,zip2,rot2,assocl]).
%lib([[SL,[SC,[ST,SR]]],H],  stepRight,  [[[SC,SL],[ST,SR]],H],
%    [assocr,rot2,zip2,assocl]).
%lib([[SL,[SC,SR]],[HL,HR]],  swapStack,  [[SL,[HR,SR]],[HL,SC]],
%    [swap,zip2,rot3,rot2,zip2,swap]).
%lib([[SL,[SC,SR]],H],  stackToElem  ,[[SL,[[SC,unit],SR]],H],
%    [assocr,intro1,rot3,zip2,assocl,rot2,assocl]).
%lib([[SL,[[SC,unit],SR]],H],  elemToStack  ,[[SL,[SC,SR]],H],
%    [swap,rot3,assocr,rot2,elim1,rot3,rot3,swap]).


%lib(X,  wrapEnv,  WX, 
%    [intro1,swap,intro1,swap,intro1,intro1,intro1,assocl,swap])
%    :- wrapped(X,WX).
%lib(WX, unwrapEnv, X,
%    [swap,assocr,elim1,elim1,elim1,swap,elim1,swap,elim1])
%    :- wrapped(X,WX).




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

urot3(N) :- path([c,[a,[b,d]]], [a,[b,[c,d]]], N).
rot4(N) :- path([a,[b,[c,[d,e]]]], [d,[a,[b,[c,e]]]], N).
rot5(N) :- path([a,[b,[c,[d,[e,f]]]]], [e,[a,[b,[c,[d,f]]]]], N).
rot6(N) :- path([a,[b,[c,[d,[e,[f,g]]]]]], [f,[a,[b,[c,[d,[e,g]]]]]], N).


intro1S(N) :- path([[sL,[sC,sR]],[hL,hR]], [[sL,[[unit,sC],sR]],[hL,hR]],N).
elim1S(N) :- path([[sL,[[unit,sC],sR]],[hL,hR]],[[sL,[sC,sR]],[hL,hR]],N).


foil(N) :- path([[a,b],[c,d]], [[a,d],[b,c]], N).

insert(N) :- path( [x,[[sL,[sC,sR]],[hL,hR]]],
                   [[sL,[[x,sC],sR]],[hL,hR]], N).
extract(N) :- path([[sL,[[x,sC],sR]],[hL,hR]],  
                   [x,[[sL,[sC,sR]],[hL,hR]]], N).
take(N) :- path( [[sL,[[x,sC],sR]],[hL,hR]],
                 [[sL,[sC,sR]],[hL,[x,hR]]], N).
put(N) :-  path( [[sL,[sC,sR]],[hL,[x,hR]]], 
                 [[sL,[[x,sC],sR]],[hL,hR]], N).


takeL(N) :- path( [[sL,[[x,sC],sR]],[hL,hR]],
                 [[sL,[sC,sR]],[[x,hL],hR]], N).
putL(N) :-  path( [[sL,[sC,sR]],[[x,hL],hR]], 
                 [[sL,[[x,sC],sR]],[hL,hR]], N).


%extract(N) :- lib([[SL,[[X,SC],SR]],H],  extract  ,[X,[[SL,[SC,SR]],H]],
%    [swap,rot3,zip2,rot3,rot2,swap,rot2]).

newStack(N) :- path([[sL,[sC,sR]],h], 
                    [[sL,[sC,[unit,sR]]],h], N).
remStack(N) :- path([[sL,[sC,[unit,sR]]],h],
                    [[sL,[sC,sR]],h], N).
stepLeft(N) :- path([[[sT,sL],[sC,sR]],h],
                    [[sL,[sT,[sC,sR]]],h], N).
stepRight(N) :- path( [[sL,[sC,[sT,sR]]],h],
                      [[[sC,sL],[sT,sR]],h], N).  
swapStackHand(N) :- path( [[sL,[sC,sR]],[hL,hR]],
                          [[sL,[hR,sR]],[hL,sC]], N).
stackToElement(N) :- path( [[sL,[sC,sR]],h],
                        [[sL,[[sC,unit],sR]],h], N).
elementToStack(N) :- path( [[sL,[[sC,unit],sR]],h],
                        [[sL,[sC,sR]],h], N). 

% open top stack item onto stack
expand(N) :- path( [[sL,[[[x,y],sC],sR]],h],
                    [[sL,[[x,[y,sC]],sR]],h], 
                    N).
% recombine top elements on a stack into one element
shrink(N) :- path( [[sL,[[x,[y,sC]],sR]],h], 
                    [[sL,[[[x,y],sC],sR]],h],
                    N).
% expand element in hand.
expandH(N) :- path( [s,[hL,[[x,y],hR]]], [s,[hL,[x,[y,hR]]]], N).
shrinkH(N) :- path( [s,[hL,[x,[y,hR]]]], [s,[hL,[[x,y],hR]]], N).


juggle2(N) :- path( [[sL,[sC,sR]], [hL,[x,[y,hR]]]],
                    [[sL,[sC,sR]], [hL,[y,[x,hR]]]], N).
juggle3(N) :- path( [[sL,[sC,sR]], [hL,[x,[y,[z,hR]]]]],
                    [[sL,[sC,sR]], [hL,[z,[x,[y,hR]]]]], N).
juggle4(N) :- path( [[sL,[sC,sR]], [hL,[a,[b,[c,[d,hR]]]]]],
                    [[sL,[sC,sR]], [hL,[d,[a,[b,[c,hR]]]]]], N).


roll2(N) :- path( [[sL,[ [x,[y,sC]]  ,sR]],[hL,hR]],
                  [[sL,[ [y,[x,sC]]  ,sR]],[hL,hR]],N).
% [swap,rot3,swap,rot3,rot2,swap,rot3,rot3,swap]9

roll3(N) :- path( [[sL,[ [x,[y,[z,sC]]]  ,sR]],[hL,hR]],
                  [[sL,[ [z,[x,[y,sC]]]  ,sR]],[hL,hR]],N).

roll4(N) :- path( [[sL,[ [a,[b,[c,[d,sC]]]]  ,sR]],[hL,hR]],
                  [[sL,[ [d,[a,[b,[c,sC]]]]  ,sR]],[hL,hR]],N).

% wrap value as first element in environment
wrapped(X,[[unit,[[X,unit],unit]],[unit,unit]]).  

wrapEnv(N) :- wrapped(x,WX),!,path(x,WX,N).
unwrapEnv(N) :- wrapped(x,WX),!,path(WX,x,N).

wrapX(N) :-
    wrapped(x,WX),!,
    path([[sL,[[x,sC],sR]],[hL,hR]]
        ,[[sL,[[WX,sC],sR]],[hL,hR]], N).
unwrapX(N) :-
    wrapped(x,WX),!,
    path([[sL,[[WX,sC],sR]],[hL,hR]]
        ,[[sL,[[x,sC],sR]],[hL,hR]], N).


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

testPath(P,X) :-  smallSteps(X,_,P).

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








    

