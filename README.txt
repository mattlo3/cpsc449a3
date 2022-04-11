CPSC 449 Assignment 3

INSTALATION INSTRUCTIONS:
Download the cpsc449-a3.zop file to a directory of your choice. Once the file has 
downloaded, extract the file to a directory of your choice. Using the terminal, 
navigate into the directory of the extracted file. In this directory, using the
"ls" terminal command should show the following files:
    constraints.pl
    main.pl
    mtree.pl
    parser.pl
    README.txt

HOW TO RUN:
Now you are in the correct directory. In order to run our program, 
use the following commands to compile and run the program:

swipl -o myProg -g main -c main.pl
./myProg inputfilename outputfilename


NOTE:
inputfilename is the path of your input file
outputfilename can be anything of your choosing