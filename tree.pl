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
    % makeTree2(Input, Answer),
    open(Output,write,Stream),
    write(Stream, Answer),
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
    makeTree3([FPA, FM, TNT, MP, TNP], 0, Score, [], Tree, Tasks),
    treeToString(Tree, Score, Answer). % answer = Solution H G F E D C B A; Quality: 800
    
% param: ([constraint lists], [[prev tree], prev score], [[curr tree], curr score], [remaining tasks], return)   
% if current task is already used, test next task
makeTree3(Aids, Prev, Curr, [T|Tx], Ans) :-
    nth0(0, Curr, CPath),
    member(T, CPath),
    makeTree3(Aids, Prev, Curr, Tx, Ans).


% makeTree3(Aids, Prev, Curr, [T|Tx], Ans) :-
    
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
    

    

 
testTests(File, Path, Pen) :-
    