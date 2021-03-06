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
<<<<<<< HEAD
<<<<<<< HEAD
    makeTree2(Input, Answer),
    treeToString(Answer, Answer2),
=======
    % makeTree2(Input, Answer),
>>>>>>> parent of 77001a7 (finished makeTree)
=======
    % makeTree2(Input, Answer),
>>>>>>> parent of 77001a7 (finished makeTree)
    open(Output,write,Stream),
    write(Stream, Answer2),
    close(Stream).


% recursive function for creating tree
makeTree2(Input, Answer) :-
    readMyFile(Input, Flines),
    delTrailingSpaces(Flines, [], Flines2),
    delMember('', Flines2, Nlines),
    parseFPA(Nlines, 0, _, FPA),
    parseFM(Nlines, 0, _, FM),
    parseTNT(Nlines, 0, _, TNT),
    parseMP(Nlines, 0, _, MP),
    parseTNP(Nlines, 0, _, TNP),
    tasks(Tasks),
<<<<<<< HEAD
<<<<<<< HEAD
    maxBound(Max),
    makeTree3([FPA, FM, TNT, MP, TNP], [[], Max], [[], 0], Tasks, Tree).

    % treeToString(Tree, Score, Answer). % answer = Solution H G F E D C B A; Quality: 800
    
% param: ([constraint lists], [[prev tree], prev score], [[curr tree], curr score], [remaining tasks], return)   

% line 31 MTree.hs
makeTree3(_, Prev, Curr, T, Ans) :-
    length(T, Length),
    Length == 0,
    Ans = Prev,
    write(Prev), write("    "), write(Curr), write("    "), write(T), write("   "), write(Length), write("\n").


% line 33 MTree.hs, checking if task already in use
makeTree3(Aids, Prev, Curr, [T|Ts], Ans) :-
    write(Prev), write("    "), write(Curr), write("    "), write([T|Ts]), write("\n"), 
=======
    makeTree3([FPA, FM, TNT, MP, TNP], 0, Score, [], Tree, Tasks),
    treeToString(Tree, Score, Answer). % answer = Solution H G F E D C B A; Quality: 800
    
% param: ([constraint lists], [[prev tree], prev score], [[curr tree], curr score], [remaining tasks], return)   
% if current task is already used, test next task
makeTree3(Aids, Prev, Curr, [T|Tx], Ans) :-
>>>>>>> parent of 77001a7 (finished makeTree)
    nth0(0, Curr, CPath),
    member(T, CPath),
    makeTree3(Aids, Prev, Curr, Tx, Ans).


<<<<<<< HEAD
% line 37 MTree.hs
makeTree3(Aids, Prev, Curr, [T|_], Ans) :-
    nth0(0, Curr, CPath),
    nth0(1, Curr, CScore),
    nth0(1, Prev, PScore),
    appendElem(CPath, T, CPath2),
    doTests(CPath2, Aids, Pen),
    length(CPath2, Length),
    Length >= 8,
    Pen2 is Pen + CScore,
    Pen2 >= PScore,
    Ans = Prev.

% line 38 MTree.hs
makeTree3(Aids, Prev, Curr, [T|Ts], Ans) :-
    nth0(0, Curr, CPath),
    nth0(1, Curr, CScore),
    nth0(1, Prev, PScore),
    appendElem(CPath, T, CPath2),
    doTests(CPath2, Aids, Pen),
    Pen2 is Pen + CScore,
    tasks(Tasks),
    makeTree3(Aids, Prev, [CPath2, Pen2], Tasks, Ans1),
    nth0(1, Ans1, AScore),
    AScore < PScore,
    AScore > 0,
    makeTree3(Aids, Ans1, Curr, Ts, Ans).

makeTree3(Aids, Prev, Curr, [T|Ts], Ans) :-
    nth0(0, Curr, CPath),
    nth0(1, Curr, CScore),
    nth0(1, Prev, PScore),
    appendElem(CPath, T, CPath2),
    doTests(CPath2, Aids, Pen),
    Pen2 is Pen + CScore,
    tasks(Tasks),
    makeTree3(Aids, Prev, [CPath2, Pen2], Tasks, Ans1),
    nth0(1, Ans1, AScore),
    AScore < PScore,
    AScore < 0,
    makeTree3(Aids, Prev, Curr, Ts, Ans).

% line 39 MTree.hs
makeTree3(Aids, Prev, Curr, [T|Ts], Ans) :-
    %snd (makeTree2 aids prev (((fst curr)++[x]), ((snd curr) + doTests ((fst curr)++[x]) aids)) tasks) >= (snd prev) 
    nth0(0, Curr, CPath),
    nth0(1, Curr, CScore),
    nth0(1, Prev, PScore),
    appendElem(CPath, T, CPath2),
    doTests(CPath2, Aids, Pen),
    Pen2 is Pen + CScore,
    tasks(Tasks),
    makeTree3(Aids, Prev, [CPath2, Pen2], Tasks, Ans1),
    nth0(1, Ans1, AScore),
    AScore >= PScore,
    makeTree3(Aids, Prev, Curr, Ts, Ans).
    %= makeTree2 aids prev curr xs
=======
% makeTree3(Aids, Prev, Curr, [T|Tx], Ans) :-
>>>>>>> parent of 77001a7 (finished makeTree)
=======
    makeTree3([FPA, FM, TNT, MP, TNP], 0, Score, [], Tree, Tasks),
    treeToString(Tree, Score, Answer). % answer = Solution H G F E D C B A; Quality: 800
    
% param: ([constraint lists], [[prev tree], prev score], [[curr tree], curr score], [remaining tasks], return)   
% if current task is already used, test next task
makeTree3(Aids, Prev, Curr, [T|Tx], Ans) :-
    nth0(0, Curr, CPath),
    member(T, CPath),
    makeTree3(Aids, Prev, Curr, Tx, Ans).


% makeTree3(Aids, Prev, Curr, [T|Tx], Ans) :-
>>>>>>> parent of 77001a7 (finished makeTree)
    
%[[],[],['11111111','11111111'],[],[]]
minBound(X) :-
    X = -123456789.

% do all constraint tests

% line 43 MTree.hs
doTests(Path, Cnst, Pen) :- %
    length(Path, M),
    nth1(M, Path, T),
    number_string(M, M2),   % M2 is string
    atom_chars(M2, Chars),  
    nth0(0, Chars, M3),     % M3 is Char
    atom_concat(M3, T, MT),  % MT = (machine, task) pair
    nth0(0, Cnst, FPA),
    forcedPartial(MT, FPA, Bool),
    Bool == 0,
    minBound(Pen).

% line 44 MTree.hs
doTests(Path, Cnst, Pen) :-
    length(Path, M),
    nth1(M, Path, T),
    number_string(M, M2),   % M2 is string
    atom_chars(M2, Chars),  
    nth0(0, Chars, M3),     % M3 is Char
    atom_concat(M3, T, MT),  % MT = (machine, task) pair
    nth0(1, Cnst, FM),
    forbidden(MT, FM),
    minBound(Pen).
    
% line 45 MTree.hs
doTests(Path, Cnst, Pen) :-
    length(Path, M),
    nth1(M, Path, T),
    M == 1,                 % M is int
    number_string(M, M2),   % M2 is string
    atom_chars(M2, Chars),  
    nth0(0, Chars, M3),     % M3 is Char
    atom_concat(M3, T, MT),  % MT = (machine, task) pair
    nth0(3, Cnst, MP),
    machPen(MT, MP, Pen).

% line 46 MTree.hs
doTests(Path, Cnst, Pen) :- 
    length(Path, M),
    nth1(1, Path, F),
    nth1(M, Path, L),
    M == 8,
    atom_concat(L, F, TT),
    nth0(2, Cnst, TNT),
    tooNear(TT,TNT),
    minBound(Pen).

% line 47 MTree.hs
doTests(Path, Cnst, Pen) :-
    length(Path, M),
    SL = M - 1,         % second last index
    nth1(M, Path, T),   % last task
    nth1(SL, Path, SLT),% second last task
    atom_concat(SLT, T, TT),
    nth0(2, Cnst, TNT),
    tooNear(TT,TNT),
    minBound(Pen).

doTests(Path, Cnst, Pen) :-
    length(Path, M),
    M == 8,
    SL = M - 1,         % second last index
    nth1(M, Path, T),   % last task
    nth1(SL, Path, SLT),% second last task
    nth1(1, Path, F),
    nth0(3, Cnst, MP),
    nth0(4, Cnst, TNP),
    atom_concat(M3, T, MT),
    machPen(MT, MP, P1),
    tooNearPen(SLT, T, TNP, P2),
    tooNearPen(T, F, TNP, P3),
    Pen = P1 + P2 + P3.

% line 48 MTree.hs
doTests(Path, Cnst, Pen) :-
    length(Path, M),
    SL = M - 1,         % second last index
    nth1(M, Path, T),   % last task
    nth1(SL, Path, SLT),% second last task
    nth0(3, Cnst, MP),
    nth0(4, Cnst, TNP),
    atom_concat(M3, T, MT),
    machPen(MT, MP, P1),
    tooNearPen(SLT, T, TNP, P2),
    Pen = P1 + P2.
    

    
<<<<<<< HEAD
<<<<<<< HEAD
% Tree to string
treeToString(Input, Output) :- 
    nth0(0, Input, CPath),
    nth0(1, Input, CScore),
    CScore < 0,
    Output = 'No valid solution possible!'.
=======
>>>>>>> parent of 77001a7 (finished makeTree)
=======
>>>>>>> parent of 77001a7 (finished makeTree)

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
<<<<<<< HEAD
<<<<<<< HEAD
    readMyFile(File, Flines),
    delTrailingSpaces(Flines, [], Flines2),
    delMember('', Flines2, Nlines),
    parseFPA(Nlines, 0, _, X),
    parseFM(Nlines, 0, _, X2),
    parseTNT(Nlines, 0, _, X3),
    parseMP(Nlines, 0, _, X4),
    parseTNP(Nlines, 0, _, X5),
    doTests(Path, [X, X2, X3, X4, X5], Pen).

=======
    
>>>>>>> parent of 77001a7 (finished makeTree)
=======
    
>>>>>>> parent of 77001a7 (finished makeTree)
