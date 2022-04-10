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


makeTree3(Aids, Prev, Curr, [T|Tx], Ans) :-
    

minBound(X) :-
    X = -123456789.

% do all constraint tests
doTests(Path, Cnst, Pen) :-
    length(Path, M),
    nth1(M, Path, T),
    M == 1,
    nth0(0, Cnst, FPA),
    nth0(0, Cnst, FM),
    forcedPartial([M, T], FPA),
    \+ forbidden([M, T], FM),
    

    
doTests(Path, Cnst, Pen) :-
    length(Path, M),
    nth1(M, Path, T),
    % M >=2.
    % FM, FPA, TNT

    % M == 1
    % FM, FPA,

    % M = 0
    % Pen = 0




    


































