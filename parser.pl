% parser file 
:- dynamic(input/1).
:- dynamic(inputNoEmpty/1).

% lists and functions for machines and tasks
tasks(['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']).
machines(['1', '2', '3', '4', '5', '6', '7', '8']).

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


% check if labels are in the correct order
correctOrder([], State) :-
        State == 6.

correctOrder([X|Xs], State) :-
        isNameLabel(X),
        State == 0,
        correctOrder(Xs, 1).

correctOrder([X|Xs], State) :-
        isFpaLabel(X),
        State == 1,
        correctOrder(Xs, 2).

correctOrder([X|Xs], State) :-
        isFmLabel(X),
        State == 2,
        correctOrder(Xs, 3).

correctOrder([X|Xs], State) :-
        isTntLabel(X),
        State == 3,
        correctOrder(Xs, 4). 

correctOrder([X|Xs], State) :-
        isMpLabel(X),
        State == 4,
        correctOrder(Xs, 5).

correctOrder([X|Xs], State) :-
        isTnpLabel(X),
        State == 5,
        correctOrder(Xs, 6).

correctOrder([_|Xs], State) :-
        correctOrder(Xs, State).

% functions to parse data into data lists

% NAME
parseName([_|Xs], State) :-
        State == 0,             % reading name label
        parseName(Xs, 1).

parseName([X|Xs], State) :-
        State == 1,             % reading name data
        atom_chars(X, Chars),
        \+ member(' ', Chars),  % no spaces in name
        parseName(Xs, 2).

parseName([X|_], State) :-
        State == 2,             % checking next label
        isFpaLabel(X).          % is FPA label

% --------------------------------------------------------------------------
% -- FPA -------------------------------------------------------------------
% --------------------------------------------------------------------------
parseFPA([X|Xs], State, Out, NOut) :- % reading file until we reach FPA label
        State == 0,        
        \+ isFpaLabel(X),
        parseFPA(Xs, 0, Out, NOut).

parseFPA([X|Xs], State, Out, NOut) :- % current line is FPA label
        State == 0,
        Out = [],
        isFpaLabel(X),
        parseFPA(Xs, 1, Out, NOut).

parseFPA([X|Xs], State, Out, NOut) :- % reading data, correct machine, correct task
        State == 1,
        \+ isFmLabel(X),
        atom_chars(X, Chars),
        nth0(1, Chars, Mach),
        nth0(3, Chars, Task),
        isMachine(Mach),        
        isTask(Task),
        atom_chars(Pair, [Mach, Task]),
        appendElem(Out, Pair, NewOut),
        parseFPA(Xs, State, NewOut, NOut).
        
parseFPA([X|_], State, Out, NOut) :- % wrong machine or wrong task
        State == 1,
        \+ isFmLabel(X),
        atom_chars(X, Chars),
        nth0(1, Chars, Mach),
        nth0(3, Chars, Task),
        ((\+ isMachine(Mach)) ; (\+ isTask(Task))),
        appendElem(Out, '0Z', NewOut),
        NOut = NewOut.

parseFPA([X|_], _, Out, NOut) :- % at FM label
        NOut = Out,
        isFmLabel(X).

hasDupe([X|Xs]) :-
        \+ hasDupe2(X, Xs),
        hasDupe(Xs).

hasDupe([X|Xs]) :-
        hasDupe2(X, Xs).

hasDupe2(Pair, [X|_]) :-
        atom_chars(Pair, PChars),
        atom_chars(X, XChars),
        nth0(0, PChars, PMach),
        nth0(1, PChars, PTask),
        nth0(0, XChars, XMach),
        nth0(1, XChars, XTask),
        number_string(PInt,[PMach]),
        number_string(XInt, [XMach]),
        ((PInt == XInt) ; ([PTask] == [XTask])).

hasDupe2(Pair, [X|Xs]) :-
        atom_chars(Pair, PChars),
        atom_chars(X, XChars),
        nth0(0, PChars, PMach),
        nth0(1, PChars, PTask),
        nth0(0, XChars, XMach),
        nth0(1, XChars, XTask),
        number_string(PInt,[PMach]),
        number_string(XInt, [XMach]),
        PInt =\= XInt,
        [PTask] =\= [XTask],
        hasDupe2(Pair, Xs).


% --------------------------------------------------------------------------
% -- FM --------------------------------------------------------------------
% --------------------------------------------------------------------------
parseFM([X|Xs], State, Out, NOut) :- % reading file until we reach FM label
        State == 0,        
        \+ isFmLabel(X),
        parseFM(Xs, 0, Out, NOut).

parseFM([X|Xs], State, Out, NOut) :- % current line is FM label
        State == 0,
        Out = [],
        isFmLabel(X),
        parseFM(Xs, 1, Out, NOut).

parseFM([X|Xs], State, Out, NOut) :- % reading data, correct machine, correct task
        State == 1,
        \+ isTntLabel(X),
        atom_chars(X, Chars),
        nth0(1, Chars, Mach),
        nth0(3, Chars, Task),
        isMachine(Mach),        
        isTask(Task),
        atom_chars(Pair, [Mach, Task]),
        appendElem(Out, Pair, NewOut),
        parseFM(Xs, State, NewOut, NOut).
        
parseFM([X|_], State, Out, NOut) :- % wrong machine or wrong task
        State == 1,
        \+ isTntLabel(X),
        atom_chars(X, Chars),
        nth0(1, Chars, Mach),
        nth0(3, Chars, Task),
        ((\+ isMachine(Mach)) ; (\+ isTask(Task))),
        appendElem(Out, '0Z', NewOut),
        NOut = NewOut.

parseFM([X|_], _, Out, NOut) :- % at TNT label
        NOut = Out,
        isTntLabel(X).

% --------------------------------------------------------------------------
% -- TNT -------------------------------------------------------------------
% --------------------------------------------------------------------------
parseTNT([X|Xs], State, Out, NOut) :- % reading file until we reach TNT label
        State == 0,        
        \+ isTntLabel(X),
        parseTNT(Xs, 0, Out, NOut).

parseTNT([X|Xs], State, Out, NOut) :- % current line is TNT label
        State == 0,
        Out = [],
        isTntLabel(X),
        parseTNT(Xs, 1, Out, NOut).

parseTNT([X|Xs], State, Out, NOut) :- % reading data, correct tasks
        State == 1,
        \+ isMpLabel(X),
        atom_chars(X, Chars),
        nth0(1, Chars, Task1),
        nth0(3, Chars, Task2),
        isTask(Task1),        
        isTask(Task2),
        atom_chars(Pair, [Task1, Task2]),
        appendElem(Out, Pair, NewOut),
        parseTNT(Xs, State, NewOut, NOut).
        
parseTNT([X|_], State, Out, NOut) :- % wrong task
        State == 1,
        \+ isMpLabel(X),
        atom_chars(X, Chars),
        nth0(1, Chars, Task1),
        nth0(3, Chars, Task2),
        ((\+ isTask(Task1)) ; (\+ isTask(Task2))),
        appendElem(Out, '0Z', NewOut),
        NOut = NewOut.

parseTNT([X|_], _, Out, NOut) :- % at MP label
        NOut = Out,
        isMpLabel(X).

% --------------------------------------------------------------------------
% -- MP --------------------------------------------------------------------
% --------------------------------------------------------------------------
parseMP([X|Xs], State, Out, NOut) :-
        State == 0,        
        \+ isMpLabel(X),
        parseMP(Xs, 0, Out, NOut).

parseMP([X|Xs], State, Out, NOut) :-
        State == 0,
        Out = [],
        isMpLabel(X),
        parseMP(Xs, 1, Out, NOut).

parseMP([X|Xs], State, Out, NOut) :- % reading data, correct data
        State >= 1,
        NewState is State + 1,
        \+ isTnpLabel(X),
        split_string(X, ' ', ' ', SNums),
        length(SNums, Length),
        Length == 8,
        nth0(0, SNums, Str1), nth0(1, SNums, Str2), nth0(2, SNums, Str3), nth0(3, SNums, Str4), nth0(4, SNums, Str5), nth0(5, SNums, Str6), nth0(6, SNums, Str7), nth0(7, SNums, Str8),
        number_string(Num1, Str1), number_string(Num2, Str2), number_string(Num3, Str3), number_string(Num4, Str4), number_string(Num5, Str5), number_string(Num6, Str6), number_string(Num7, Str7), number_string(Num8, Str8),
        Num1 >= 0, Num2 >= 0, Num3 >= 0, Num4 >= 0, Num5 >= 0, Num6 >= 0, Num7 >= 0, Num8 >= 0,    
        integer(Num1), integer(Num2), integer(Num3), integer(Num4), integer(Num5), integer(Num6), integer(Num7), integer(Num8), 
        appendElem(Out, [Num1, Num2, Num3, Num4, Num5, Num6, Num7, Num8], NewOut),
        parseMP(Xs, NewState, NewOut, NOut).

parseMP([X|_], State, Out, NOut) :- % reading data, machine penalty
        State >= 1,
        \+ isTnpLabel(X),
        split_string(X, ' ', ' ', SNums),
        length(SNums, Length),
        Length =\= 8,
        appendElem(Out, '1Z', NewOut),
        NOut = NewOut.

parseMP([X|_], State, Out, NOut) :- % reading data, invalid penalty
        State >= 1,
        \+ isTnpLabel(X),
        split_string(X, ' ', ' ', SNums),
        length(SNums, Length),
        Length == 8,
        nth0(0, SNums, Str1), nth0(1, SNums, Str2), nth0(2, SNums, Str3), nth0(3, SNums, Str4), nth0(4, SNums, Str5), nth0(5, SNums, Str6), nth0(6, SNums, Str7), nth0(7, SNums, Str8),
        number_string(Num1, Str1), number_string(Num2, Str2), number_string(Num3, Str3), number_string(Num4, Str4), number_string(Num5, Str5), number_string(Num6, Str6), number_string(Num7, Str7), number_string(Num8, Str8),
        ((\+ Num1 >= 0) ; (\+ Num2 >= 0) ; (\+ Num3 >= 0) ; (\+ Num4 >= 0) ; (\+ Num5 >= 0) ; (\+ Num6 >= 0) ; (\+ Num7 >= 0) ; (\+ Num8 >= 0) ; (\+ integer(Num1)) ; (\+ integer(Num2)) ; (\+ integer(Num3)) ; (\+ integer(Num4)) ; (\+ integer(Num5)) ; (\+ integer(Num6)) ; (\+ integer(Num7)) ; (\+ integer(Num8))), 
        appendElem(Out, '0Z', NewOut),
        NOut = NewOut.

parseMP([X|_], State, Out, NOut) :- % not 8 lines, machine penalty
        isTnpLabel(X),
        State =\= 9,
        appendElem(Out, '1Z', NewOut),
        NOut = NewOut.

parseMP([X|_], State, Out, NOut) :- % TNP label
        isTnpLabel(X),
        State == 9,
        NOut = Out.

%TNP-----------------------------------------------------------------------
parseTNP([X|Xs], State, Out, NOut) :- % reading file until we reach TNT label
        State == 0,        
        \+ isTnpLabel(X),
        parseTNP(Xs, 0, Out, NOut).

parseTNP([X|Xs], State, Out, NOut) :- % current line is TNT label
        State == 0,
        Out = [],
        isTnpLabel(X),
        parseTNP(Xs, 1, Out, NOut).

parseTNP([X|Xs], State, Out, NOut) :- % reading data, correct tasks
        State == 1,
        length([X], Length),
        \+ (Length == 0),
        atom_chars(X, Chars),
        nth0(1, Chars, Task1),
        nth0(3, Chars, Task2),
        nth0(5, Chars, Pen),
        number_string(Num1, [Pen]),
        isTask(Task1),        
        isTask(Task2),
        integer(Num1),
        Num1 >= 0,
        atom_chars(Pair, [Task1, Task2, Pen]),
        appendElem(Out, Pair, NewOut),
        parseTNP(Xs, State, NewOut, NOut).
        
parseTNP([X|_], State, Out, NOut) :- % penalty error (negative int)
        State == 1,
        length([X], Length),
        \+ (Length == 0),
        atom_chars(X, Chars),
        nth0(1, Chars, Task1),
        nth0(3, Chars, Task2),
        nth0(5, Chars, Pen),
        isTask(Task1),        
        isTask(Task2),
        \+ number_string(_, [Pen]),
        appendElem(Out, '1Z', NewOut),
        NOut = NewOut.

parseTNP([X|_], State, Out, NOut) :- % penalty error (not int)
        State == 1,
        length([X], Length),
        \+ (Length == 0),
        atom_chars(X, Chars),
        nth0(1, Chars, Task1),
        nth0(3, Chars, Task2),
        nth0(5, Chars, Pen),
        isTask(Task1),        
        isTask(Task2),
        \+ number_string(_, [Pen]),
        appendElem(Out, '1Z', NewOut),
        NOut = NewOut.

parseTNP([X|_], State, Out, NOut) :- % invalid task
        State == 1,
        length([X], Length),
        \+ (Length == 0),
        atom_chars(X, Chars),
        nth0(1, Chars, Task1),
        nth0(3, Chars, Task2),
        ((\+ isTask(Task1)) ; (\+ isTask(Task2))),
        appendElem(Out, '0Z', NewOut),
        NOut = NewOut.

parseTNP(X, _, Out, NOut) :- % at end of file
        length(X,Length),
        Length == 0,
        NOut = Out.




%---------------error handling--------------------------------------------------------

%        parseName(Nlines, 0),
%        parseFPA(Nlines, 0, FPADown, FPAUp),
%        parseFM(Nlines, 0, FMDown, FMUp),
%        parseTNT(Nlines, 0, TNTDown, TNTUp),
%        parseMP(Nlines, 0, MPDown, MPUp),
%        parseTNP(Nlines, 0, TNPDown, TNPUp),


% --------------------------------------------------------------------------
% MAIN PARSE FUNCTION
% --------------------------------------------------------------------------
%  ERRORS
% 0 = Error while parsing input file
% -1 = invalid machine/task
% -2 = partial assignment error
% -3 = machine penalty error
% -4 = invalid penalty
% -5 = invalid task

mainParse(File, Parsed) :- % if not correct order
        readMyFile(File, Flines),
        delTrailingSpaces(Flines, [], Flines2),
        delMember('', Flines2, Nlines),
        \+ correctOrder(Nlines, 0),
        Parsed = 0.

mainParse(File, Parsed) :- % if not correct name
        readMyFile(File, Flines),
        delTrailingSpaces(Flines, [], Flines2),
        delMember('', Flines2, Nlines),
        \+ parseName(Nlines, 0),
        Parsed = 0.

mainParse(File, Parsed) :- % FPA, invalid machine/task
        readMyFile(File, Flines),
        delTrailingSpaces(Flines, [], Flines2),
        delMember('', Flines2, Nlines),
        parseFPA(Nlines, 0, _, FPAUp),
        member('0Z', FPAUp),
        Parsed = -1.

mainParse(File, Parsed) :- % FPA, invalid machine/task
        readMyFile(File, Flines),
        delTrailingSpaces(Flines, [], Flines2),
        delMember('', Flines2, Nlines),
        parseFPA(Nlines, 0, _, FPAUp),
        hasDupe(FPAUp),
        Parsed = -2.

mainParse(File, Parsed) :- % FM, invalid machine/task
        readMyFile(File, Flines),
        delTrailingSpaces(Flines, [], Flines2),
        delMember('', Flines2, Nlines),
        parseFM(Nlines, 0, _, FMUp),
        member('0Z', FMUp),
        Parsed = -1.

mainParse(File, Parsed) :- % TNT, invalid machine/task
        readMyFile(File, Flines),
        delTrailingSpaces(Flines, [], Flines2),
        delMember('', Flines2, Nlines),
        parseTNT(Nlines, 0, _, TNTUp),
        member('0Z', TNTUp),
        Parsed = -1.

mainParse(File, Parsed) :- % MP, invalid penalty
        readMyFile(File, Flines),
        delTrailingSpaces(Flines, [], Flines2),
        delMember('', Flines2, Nlines),
        parseMP(Nlines, 0, _, MPUp),
        member('0Z', MPUp),
        Parsed = -4.

mainParse(File, Parsed) :- % MP, machine penalty error
        readMyFile(File, Flines),
        delTrailingSpaces(Flines, [], Flines2),
        delMember('', Flines2, Nlines),
        parseMP(Nlines, 0, _, MPUp),
        member('1Z', MPUp),
        Parsed = -3.

mainParse(File, Parsed) :- % TNP , invalid task
        readMyFile(File, Flines),
        delTrailingSpaces(Flines, [], Flines2),
        delMember('', Flines2, Nlines),
        parseTNP(Nlines, 0, _, TNPUp),
        member('0Z', TNPUp),
        Parsed = -5.


mainParse(File, Parsed) :- % TNP, invalid penalty
        readMyFile(File, Flines),
        delTrailingSpaces(Flines, [], Flines2),
        delMember('', Flines2, Nlines), 
        parseTNP(Nlines, 0, _, TNPUp),
        member('1Z', TNPUp),
        Parsed = -4.

mainParse(File, Parsed) :- % no errors, valid parse
        readMyFile(File, Flines),
        delTrailingSpaces(Flines, [], Flines2),
        delMember('', Flines2, _),
        Parsed = 1.