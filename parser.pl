%parser file 

% from: https://www.newthinktank.com/2015/08/learn-prolog-one-video/ 
% Write to a file by defining the file, text to write, connection
 
% to the file (Stream)
 
write_to_file(File, Text) :-
  open(File, write, Stream),
  write(Stream, Text), nl,
  close(Stream).
 
% Read from a file
 
read_file(File) :-
        open(File, read, Stream),
 
        % Get char from the stream
        get_char(Stream, Char1),
 
        % Outputs the characters until end_of_file
        process_stream(Char1, Stream),
        close(Stream).
 
% Continue getting characters until end_of_file
 
% ! or cut is used to end backtracking or this execution
 
process_stream(end_of_file, _) :- !.
 
process_stream(Char, Stream) :-
        write(Char),
        get_char(Stream, Char2),
        process_stream(Char2, Stream).


% from: https://stackoverflow.com/questions/16086203/how-to-read-from-file-using-see-and-putting-the-content-into-a-list-in-prolog
readFile(InputFile, TextList):- open(InputFile, read, Stream),
                                readCharacter(Stream, TextList),
                                close(Stream),
                                name(Word, TextList),
                                TextList is Word, nl, 
                                !.

readCharacter(Stream,[]):- at_end_of_stream(Stream).    %condizione di uscita


readCharacter(Stream,[Char|Rest]):-
                                 get0(Stream,Char),
                                 readCharacter(Stream,Rest).

% from: https://stackoverflow.com/questions/4805601/read-a-file-line-by-line-in-prolog
read_by_line :-
    open('examples1/garbage.txt', read, Str),
    read_file(Str, Lines),
    close(Str),
    write(Lines), nl.

read_file(Stream,[]) :- at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read(Stream,X),
    read_file(Stream,L).