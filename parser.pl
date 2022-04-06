% parser file 
:- dynamic(input/1).
:- dynamic(inputNoEmpty/1).

% lists and functions for machines and tasks
tasks(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']).
machines([1, 2, 3, 4, 5, 6, 7, 8]).

isTask(X) :-
        tasks(L), member(X,L).

isMachine(X) :-
        machines(L), member(X,L).

% variables and functions for label comparison
nameLabel('Name:').
fpaLabel('forced partial assignment:').
fmLabel('forbidden machine:').
tntLabel('too-near tasks:').
mpLabel('machine penalties:').
tnpLabel('too-near penalities').
 
isNameLabel(X) :-
        nameLabel(L), X == L.
isFpaLabel(X) :-
        fpaLabel(L), X == L.
isFmLabel(X) :-
        fmLabel(L), X == L.
isTntLabel(X) :-
        tntLabel(L), X == L.
isMpLabel(X) :-
        mpLabel(L), X == L.
isTnpLabel(X) :-
        tnpLabel(L), X == L.
 
% read file and save lines to a list
% https://stackoverflow.com/questions/49443708/how-to-read-a-file-creating-a-list
my_representation(Codes, Result) :-
    atom_codes(Result, Codes).

stream_representations(Input, Lines) :-
    read_line_to_codes(Input, Line),
    (   Line == end_of_file
    ->  Lines = []
    ;   my_representation(Line, FinalLine),
        Lines = [FinalLine | FurtherLines],
        stream_representations(Input, FurtherLines) ).

readMyFile(File, Flines) :-
    open(File, read, Input),
    stream_representations(Input, Lines),
    close(Input),
    Flines = Lines.
    

% remove extra lines
% https://stackoverflow.com/questions/12175377/deleting-all-occurrences-of-an-element-from-a-list
delMember(_, [], []).
delMember(X, [X|Xs], Y) :-
    delMember(X, Xs, Y).
delMember(X, [T|Xs], [T|Y]) :-
    dif(X, T),
    delMember(X, Xs, Y).

% is last element
isLast(X, [X]).
isLast(X, [_|Z]) :-
        isLast(X, Z).

% remove last element
% https://stackoverflow.com/questions/16174681/how-to-delete-the-last-element-from-a-list-in-prolog
list_butlast([X|Xs], Ys) :-                 % use auxiliary predicate ...
   list_butlast_prev(Xs, Ys, X).            % ... which lags behind by one item

list_butlast_prev([], [], _).
list_butlast_prev([X1|Xs], [X0|Ys], X0) :-  
        list_butlast_prev(Xs, Ys, X1). 


% append lists
% https://stackoverflow.com/questions/11539203/how-do-i-append-lists-in-prolog
append( [], X, X).                                   % (* your 2nd line *)
append( [X | Y], Z, [X | W]) :- append( Y, Z, W).

% append an element
appendElem(List, X, Y) :-
        append(List, [X], Y).


% remove trailing spaces
delTrailingSpaces([], Y, Y).
delTrailingSpaces([X|Xs], Y, Result) :-
        atom_chars(X, Chars),
        delTrailingSpaces2(Chars, NewChars),    % recursive function call
        atom_chars(Word, NewChars),
        appendElem(Y, Word, Z),
        delTrailingSpaces(Xs, Z, Result).

% recursive function for removing trailing spaces from each element
delTrailingSpaces2(X, Y) :-
        isLast(' ', X), 
        list_butlast(X, Z),
        delTrailingSpaces2(Z, Y).
delTrailingSpaces2(X, Y) :- 
        \+ isLast(' ', X),
        Y = X.






mainParse(File, Parsed) :-
    readMyFile(File, Flines),
    delTrailingSpaces(Flines, [], Flines2),
    delMember('', Flines2, Nlines),
    Parsed = Nlines.
