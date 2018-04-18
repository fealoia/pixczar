PixCzar: An Animation Language

Frank Aloia - fea2113@columbia.edu
Gary Chen - gc2676@columbia.edu
Bryan Li - bl2557@columbia.edu
Matias Lirman - ml3707@columbia.edu
________________________________________________________________

How to Compile/Execute: "./testall.sh"
  testall.sh script should do the following:
  1. Compile the files and produce a executable pixczar.native
  2. Test the executable against the tests within the tests directory and return
  information on the success of the tests
    a.  - Green text indicates expected output for a test
        - Red text indicates unexpected output for a test
        -Ex: "./pixczar.native shouldfail.pxr" should print FAILURE in green

*** make.sh exports specific path of llvm found in the group's computer. If llvm
is located in another directory then the function will not build. To fix problem
please change the path to the users appropriate llvm directory ***

*** make.sh uses "-ocamlfind" and "lli" to compile source code so it is assumed
that these compiling tools are already downloaded on user's computer ***
________________________________________________________________

TESTS FOR DELIVERABLE 4/18:

(New features as of previous deliverable)

Positive Tests:
1. Array Assign - creation of array and assignment of an element
2. Array Initializtion - initialize array with elements on creation
3. For - for loop with all 3 header conditions and a body
4. If/Else - run else body when the 'if' condition fails
5. PostOps - post expressions will change value of an int and return the result
6. PreOps - pre expressions will change the value of an int and return the previous value
7. Reassign Variable - variable can be reassigned to a new value later in program
8. While - as long as the condition is true, program will run the body of the 'while' loop

Negative Tests:
1. Array Assignment Wrong Type - can't fill an array with elements of incorrect type
2. Duplicate Variables - can't have multiple variables of the same id
3. Arguments on Object Creation - can't create object with incorrect number of arguments

Previously Existing Tests:
1. HelloWorld
2. Function Header Checks
