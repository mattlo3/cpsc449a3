% takes X = [Int, Char] Y = [[Int,Char]]. if X exists in Y, then that machine/task pair is forbidde, return false
% [5,A] is in [[3,B],[5,A],[3,C]]
% 5 = machine 5, 1 =  task A


forbidden(X,Y) :-
    member(X,Y).

tooNear(X,Y) :-
    member(X,Y).

forcedPartial(X,Y) :-
    \+ member(X,Y).

