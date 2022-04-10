% takes X = [Int, Char] Y = [[Int,Char]]. if X exists in Y, then that machine/task pair is forbidde, return false
% [5,A] is in [[3,B],[5,A],[3,C]]
% 5 = machine 5, 1 =  task A

% --------------------------
% Hard Constraints
% --------------------------
% FM -----------------------
forbidden(X,Y) :-
    member(X,Y).

% TN -----------------------
tooNear(X,Y) :-
    member(X,Y).


% FPA -----------------------
forcedPartial(_, Y, R) :- % not defined
    length(Y, Length),
    Length == 0,
    R = true.

forcedPartial(X,[Y|_], R) :- % in FPA
    atom_chars(X, Pair),
    atom_chars(Y, CPair),
    nth0(0, Pair, Mach1), 
    nth0(0, CPair, Mach2), 
    nth0(1, Pair, Task1), 
    nth0(1, CPair, Task2), 
    Mach1 == Mach2, 
    Task1 == Task2,
    R = true.

forcedPartial(X,[Y|_], R) :- % in FPA
    atom_chars(X, Pair),
    atom_chars(Y, CPair),
    nth0(0, Pair, Mach1), 
    nth0(0, CPair, Mach2), 
    nth0(1, Pair, Task1), 
    nth0(1, CPair, Task2), 
    (\+ Mach1 == Mach2 , Task1 == Task2),
    R = false.
   
    
forcedPartial(X,[Y|_], R) :- % in FPA
    atom_chars(X, Pair),
    atom_chars(Y, CPair),
    nth0(0, Pair, Mach1), 
    nth0(0, CPair, Mach2), 
    nth0(1, Pair, Task1), 
    nth0(1, CPair, Task2), 
    (Mach1 == Mach2 , \+ Task1 == Task2),
    R = false.

forcedPartial(X,[_|Ys], R) :- % in FPA
    forcedPartial(X, Ys, R).
% --------------------------
% Soft Constraints
% --------------------------
% MP -----------------------
machPen(M, T, C, P) :-
    number_string(Mi, [M]),
    taskToInt(T, Ti),
    nth1(Mi, C, Row),
    nth1(Ti, Row, P).
    
% TNP -----------------------
tooNearPen(T1, T2, [C|_], P) :-
    atom_chars(C, Chars),
    member([T1, T2, _], [Chars]),
    nth0(2, Chars, Pen),
    number_string(P, [Pen]).

tooNearPen(T1, T2, [C|Cs], P) :-
    atom_chars(C, Chars),
    \+ member([T1, T2, _], [Chars]),
    tooNearPen(T1, T2, Cs, P).

tooNearPen(_, _, C, P) :-
    length(C, Length),
    Length == 0,
    P = 0.

% ----------------------------------------------------------_______
% Task To Int ----------------------------------------------|bubby|
% ----------------------------------------------------------
taskToInt(Char, Int) :-
    Char == 'A',
    Int = 1.

taskToInt(Char, Int) :-
    Char == 'B',
    Int = 2.

taskToInt(Char, Int) :-
    Char == 'C',
    Int = 3.

taskToInt(Char, Int) :-
    Char == 'D',
    Int = 4.
    
taskToInt(Char, Int) :-
    Char == 'E',
    Int = 5.

taskToInt(Char, Int) :-
    Char == 'F',
    Int = 6.
    
taskToInt(Char, Int) :-
    Char == 'G',
    Int = 7.

taskToInt(Char, Int) :-
    Char == 'H',
    Int = 8.

