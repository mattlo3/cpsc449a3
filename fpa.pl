% Forced Partial Assignment

% load in the lists library:
:- use_module(library(lists)).

girl(jaya).

% get the length of a list
list_length([], 0).
list_length([_|TAIL],N) :- list_length(TAIL,N1), N is N1 + 1.

% check a list against an index to see if it will be out of range
is_out_range(X, LIST) :- list_length(LIST, Len), X > Len.

% rule that checks a single pair with a list of tasks
in_list([X,Y], MLIST) :- is_out_range(X, MLIST); nth1(X, MLIST, Y).

% rule that iterates through and checks each constraint against the list
% returns false if there is an infraction, true otherwise
fpa([H|TAIL], MLIST) :- \+(in_list(H, MLIST)) -> false ; (TAIL == [] -> true ; fpa(TAIL, MLIST)).
