:- include('mtree.pl').

main() :-
    current_prolog_flag(argv, AllArgs),
    nth1(1, AllArgs, Input),
    nth1(2, AllArgs, Output),
    makeTree(Input,Output).