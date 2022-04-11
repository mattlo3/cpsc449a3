:- include('constraints.pl').
:- include('parser.pl').

makeTree(Input, Output) :-
    mainParse(Input, Res),
    Res == 0,
    open(Output,write,Stream),
    write(Stream,'Error while parsing input file'),
    close(Stream).

makeTree(Input, Output) :-
    mainParse(Input, Res),
    Res == -1,
    open(Output,write,Stream),
    write(Stream,'invalid machine/task'),
    close(Stream).

makeTree(Input, Output) :-
    mainParse(Input, Res),
    Res == -2,
    open(Output,write,Stream),
    write(Stream,'partial assignment error'), 
    close(Stream).
    
makeTree(Input, Output) :-
    mainParse(Input, Res),
    Res == -3,
    open(Output,write,Stream),
    write(Stream,'machine penalty error'),
    close(Stream).

makeTree(Input, Output) :-
    mainParse(Input, Res),
    Res == -4,
    open(Output,write,Stream),
    write(Stream,'invalid penalty'),  
    close(Stream).

makeTree(Input, Output) :-
    mainParse(Input, Res),
    Res == -5,
    open(Output,write,Stream),
    write(Stream,'invalid task'),
    close(Stream).


makeTree(Input, Output) :-
    mainParse(Input, Res),
    Res == 1,
    makeTree2(Input, Answer),
    treeToString(Answer, Answer2),
    open(Output,write,Stream),
    write(Stream, Answer2),
    close(Stream).


% recursive function for creating tree
makeTree2(Input, Tree) :- % dont forget to revert Tree to Answ
    readMyFile(Input, Flines),
    delTrailingSpaces(Flines, [], Flines2),
    delMember('', Flines2, Nlines),
    parseFPA(Nlines, 0, _, FPA),
    parseFM(Nlines, 0, _, FM),
    parseTNT(Nlines, 0, _, TNT),
    parseMP(Nlines, 0, _, MP),
    parseTNP(Nlines, 0, _, TNP),
    tasks(Tasks),
    maxBound(Max),
    makeTree3([FPA, FM, TNT, MP, TNP], [[], Max], [[], 0], Tasks, Tree).

%----------------------------------------------------------------------------------------
% makeTree3(constraints, prevBest, currPath, tasks, result)

% Current MT FAILS HC
makeTree3(Cnst, Prev, Curr, [T|Ts], Ans) :-
    nth0(0, Curr, CPath),
    nth0(1, Curr, CScore),
    appendElem(CPath, T, NCurr),            % add available task to path
    doTests(NCurr, Cnst, Pen),
    (Pen < 0 ; member(T, CPath)),           % if NCurr fails HC or T already in use
    makeTree3(Cnst, Prev, Curr, Ts, Ans).   % Recurse with old current path, and next available task

% No more available tasks to try
makeTree3(Cnst, Prev, Curr, T, Ans) :-
    length(T, LenT),
    LenT == 0,
    Ans = Prev.                             % return previous best sub tree

% length = 8
    % better than PREV
makeTree3(Cnst, Prev, Curr, [T|Ts], Ans) :-
    nth0(0, Curr, CPath),
    nth0(1, Curr, CScore),
    appendElem(CPath, T, NCurr),
    length(NCurr, LenNCurr),
    LenNCurr == 8,
    write(NCurr), write("\n"),
    doTests(NCurr, Cnst, Pen),
    nth0(1, Prev, PScore),
    Pen2 is Pen + CScore,
    Pen2 >= 0,
    Pen2 =< PScore, % fixes NOTHING (102), fixes invalidforbidden (102, 136), toonearpen1(131,136,102)
    Ans = [NCurr, Pen2].

    % worse than PREV
makeTree3(Cnst, Prev, Curr, [T|Ts], Ans) :-
    nth0(0, Curr, CPath),
    nth0(1, Curr, CScore),
    appendElem(CPath, T, NCurr),
    length(NCurr, LenNCurr),
    LenNCurr == 8,
    doTests(NCurr, Cnst, Pen),
    nth0(1, Prev, PScore),
    Pen2 is Pen + CScore,
    Pen2 >= 0,
    Pen2 > PScore, % fixes NOTHING (116)
    Ans = Prev.
    
% length < 8
    % better than PREV
makeTree3(Cnst, Prev, Curr, [T|Ts], Ans) :-
    nth0(0, Curr, CPath),
    nth0(1, Curr, CScore),
    appendElem(CPath, T, NCurr),
    length(NCurr, LenNCurr),
    LenNCurr < 8,
    doTests(NCurr, Cnst, Pen),
    nth0(1, Prev, PScore),
    Pen2 is Pen + CScore,
    Pen2 >= 0,
    Pen2 =< PScore, % fixes NOTHING (131), toonearpen1(131,136,102)
    tasks(Tasks),
    makeTree3(Cnst, Prev, [NCurr, Pen2], Tasks, Ans1),
    nth0(1, Ans1, AScore),
    AScore >= 0,
    AScore =< PScore, % fixes invalidtoonear, invalid2, toonearpen2 (136), invalidforbidden(102, 136), toonearpen1(131,136,102)
    makeTree3(Cnst, Ans1, Curr, Ts, Ans).

    % worse than PREV
makeTree3(Cnst, Prev, Curr, [T|Ts], Ans) :-
    nth0(0, Curr, CPath),
    nth0(1, Curr, CScore),
    appendElem(CPath, T, NCurr),
    length(NCurr, LenNCurr),
    LenNCurr < 8,
    doTests(NCurr, Cnst, Pen),
    nth0(1, Prev, PScore),
    Pen2 is Pen + CScore,
    Pen2 >= 0,
    Pen2 =< PScore,  % fixes NOTHING (150)
    tasks(Tasks),
    makeTree3(Cnst, Prev, [NCurr, Pen2], Tasks, Ans1),
    nth0(1, Ans1, AScore),
    (AScore < 0 ; AScore > PScore),
    makeTree3(Cnst, Prev, Curr, Ts, Ans).

%----------------------------------------------------------------------------------------
% general doTests
doTests(Path, Cnst, Pen) :-         % path is size 1
    length(Path, LenPath),
    LenPath == 1,
    doTests1(Path, Cnst, Pen).    

doTests(Path, Cnst, Pen) :-         % if path is full
    length(Path, LenPath),
    LenPath == 8,
    doTests8(Path, Cnst, Pen).

doTests(Path, Cnst, Pen) :-         % if path is not full and not 1 
    length(Path, LenPath),
    LenPath > 1,
    LenPath < 8,
    doTestsN(Path, Cnst, Pen).

% Specialized doTest Functions
% size 1, FAIL HC
doTests1(Path, Cnst, Pen) :-                                        
    nth0(0, Cnst, FPA),
    nth0(1, Cnst, FM),
    nth1(1, Path, Task),
    atom_concat('1', Task, MT),     % machine/task pair '1T'
    forcedPartial(MT, FPA, Bool),
    (Bool == 0 ; forbidden(MT, FM)),% if FPA or FM fail
    minBound(Pen).                  % pen = -123456789

% size 1, PASS HC
doTests1(Path, Cnst, Pen) :-                                        
    nth0(0, Cnst, FPA),
    nth0(1, Cnst, FM),
    nth1(1, Path, Task),
    atom_concat('1', Task, MT),     % machine/task pair '1T'
    forcedPartial(MT, FPA, Bool),
    Bool == 1,                      % if FPA pass
    \+ forbidden(MT, FM),           % if FM pass
    nth0(3, Cnst, MP),
    machPen(MT, MP, Pen).           % calculate pen = mp

% size 8, FAIL HC
doTests8(Path, Cnst, Pen) :-                                        
    nth0(0, Cnst, FPA),
    nth0(1, Cnst, FM),
    nth0(2, Cnst, TNT),
    nth1(8, Path, LT),              % last task
    nth1(7, Path, SL),              % second last task
    nth1(1, Path, FT),              % first task
    atom_concat('8', LT, MT),       % Mach, Task pair
    atom_concat(LT, FT, LF),        % last, first task pair
    atom_concat(SL, LT, SLP),       % second last, last task pair
    forcedPartial(MT, FPA, Bool),
    (Bool == 0 ; forbidden(MT, FM) ; tooNear(SLP, TNT) ; tooNear(LF, TNT)),
    minBound(Pen).

% size 8, PASS HC
doTests8(Path, Cnst, Pen) :-
    nth0(0, Cnst, FPA),
    nth0(1, Cnst, FM),
    nth0(2, Cnst, TNT),
    nth1(8, Path, LT),              % last task
    nth1(7, Path, SL),              % second last task
    nth1(1, Path, FT),              % first task
    atom_concat('8', LT, MT),       % Mach, Task pair
    atom_concat(LT, FT, LF),        % last, first task pair
    atom_concat(SL, LT, SLP),       % second last, last task pair
    forcedPartial(MT, FPA, Bool),
    Bool == 1,
    \+ forbidden(MT, FM),
    \+ tooNear(SLP, TNT),
    \+ tooNear(LF, TNT),
    nth0(3, Cnst, MP),
    machPen(MT, MP, P1),
    nth0(4, Cnst, TNP),
    tooNearPen(SL, LT, TNP, P2),
    tooNearPen(LT, FT, TNP, P3),
    Pen is P1 + P2 + P3.

%  1 < Size < 8, FAIL HC
doTestsN(Path, Cnst, Pen) :-
    nth0(0, Cnst, FPA),
    nth0(1, Cnst, FM),
    nth0(2, Cnst, TNT),
    length(Path, LM),               % LM = last machine
    SLM is LM - 1,                  % SLM = second last machine
    nth1(LM, Path, LT),             % LT = last task
    nth1(SLM, Path, SLT),           % SLT = second last task
    atom_concat(SLT, LT, SLP),
    number_string(LM, M),
    atom_chars(M, MChars),
    nth0(0, MChars, M2),
    atom_concat(M2, LT, MT),
    forcedPartial(MT, FPA, Bool),
    (Bool == 0 ; forbidden(MT, FM) ; tooNear(SLP, TNT)),
    minBound(Pen).
    
%  1 < Size < 8, PASS HC
doTestsN(Path, Cnst, Pen) :-
    nth0(0, Cnst, FPA),
    nth0(1, Cnst, FM),
    nth0(2, Cnst, TNT),
    length(Path, LM),               % LM = last machine
    SLM is LM - 1,                  % SLM = second last machine
    nth1(LM, Path, LT),             % LT = last task
    nth1(SLM, Path, SLT),           % SLT = second last task
    atom_concat(SLT, LT, SLP),
    number_string(LM, M),
    atom_chars(M, MChars),
    nth0(0, MChars, M2),
    atom_concat(M2, LT, MT),
    forcedPartial(MT, FPA, Bool),
    Bool == 1, 
    \+ forbidden(MT, FM),
    \+ tooNear(SLP, TNT),
    nth0(3, Cnst, MP),
    machPen(MT, MP, P1),
    nth0(4, Cnst, TNP),
    tooNearPen(SLT, LT, TNP, P2),
    Pen is P1 + P2.
%----------------------------------------------------------------------------------------

minBound(X) :-
    X = -123456789.

maxBound(X) :-
    X = 123456789.

% Tree to string
treeToString(Input, Output) :- 
    nth0(0, Input, CPath),
    nth0(1, Input, CScore),
    CScore < 0,
    Output = 'No valid solution possible!'.

treeToString(Input, Output) :- 
    nth0(0, Input, CPath),
    nth0(1, Input, CScore),
    nth1(1, CPath, T1),
    nth1(2, CPath, T2),
    nth1(3, CPath, T3),
    nth1(4, CPath, T4),
    nth1(5, CPath, T5),
    nth1(6, CPath, T6),
    nth1(7, CPath, T7),
    nth1(8, CPath, T8),
    CScore >= 0,
    atom_concat('Solution ', T1, S1),
    atom_concat(S1, ' ', S2),
    atom_concat(S2, T2, S3),
    atom_concat(S3, ' ', S4),
    atom_concat(S4, T3, S5),
    atom_concat(S5, ' ', S6),
    atom_concat(S6, T4, S7),
    atom_concat(S7, ' ', S8),
    atom_concat(S8, T5, S9),
    atom_concat(S9, ' ', S10),
    atom_concat(S10, T6, S11),
    atom_concat(S11, ' ', S12),
    atom_concat(S12, T7, S13),
    atom_concat(S13, ' ', S14),
    atom_concat(S14, T8, S15),
    atom_concat(S15, '; Quality: ', S16),
    atom_concat(S16, CScore, S17),
    Output = S17.
 
testTests(File, Path, Pen) :-
    readMyFile(File, Flines),
    delTrailingSpaces(Flines, [], Flines2),
    delMember('', Flines2, Nlines),
    parseFPA(Nlines, 0, _, X),
    parseFM(Nlines, 0, _, X2),
    parseTNT(Nlines, 0, _, X3),
    parseMP(Nlines, 0, _, X4),
    parseTNP(Nlines, 0, _, X5),
    doTests(Path, [X, X2, X3, X4, X5], Pen). 