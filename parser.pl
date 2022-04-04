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

delTrailSpace().

mainParse(File, Parsed) :-
    readMyFile(File, Flines),
    delMember('', Flines, Nlines),
    Parsed = Nlines.