#|
                    ***** 8PUZZLE.LSP *****

8 Puzzle program for Artificial Intelligence.

**8-Puzzle Description**
In the study of Artificial Intelligence, the 8-puzzle is a 
simple sliding puzzle "toy" problem used to illustrate the 
concepts of search space. To solve this puzzle, 8 tiles are 
repositioned about a 3x3 grid in a sliding fashion in order 
to acheive a goal state. These puzzles are represented in 
row-major form, with the tiles being represented as 1-8, with 
the space being represented as a 0. A standard 8-puzzle game is 
simulated below:

    1 3 4       1 3 4       1 3 4       1 3         
    8 6 2   ->  8   2   ->  8 2     ->  8 2 4   ->  
    7   5       7 6 5       7 6 5       7 6 5       

    1   3       1 2 3       
    8 2 4   ->  8   4   <- (This is the goal state!)    
    7 6 5       7 6 5       

**Program Objective**
The objective of this assignment is to use the Lisp programming 
language to solve the 8-puzzle using Breadth-First Search (BFS), 
Depth First Iterated Deepening (DFID), and A*, a heuristics-based 
search method.

**BFS Strategy**
Breadth-First Search is a standard algorithm for searching graph 
structures. From a given start state, this algorithm searches 
neighboring nodes of the same level first, before exploring 
neighbors present in the next level.

          (1)
         / | \
       (2)(3)(4)
       /   |  | \
     (5)  (6)(7)(8)
      |
     (9)

**DFID Strategy**
Depth First Iterated Deepening is a state spaace search strategy 
in which there is a depth limit to the Depth-First Search (DFS) 
with increasing depth limits until a goal state is reached. This 
allows for a version of depth-first search similar to BFS but with 
much smaller memory requirements. 

**A* Search Strategy**
The A* search algorithm is a version of Dijkstra's algorithm that 
performs better than exhaustive searches in certain situations due to 
its use of heuristics to guide search.
As the search algorithm is running, A* determines the next node to 
expand by determining the estimate of the cost or weight to reach 
the goal state. This is done by using the following equation:

f(n) = g(n) + h(n)

Where n is the node on the path, g(n) is the cost from the start 
node to the given n, and h(n) is the heuristic value that estimates 
the remaining cost from n to the goal state.

**Heuristics**
Heuristics can be both admissible and inadmissible. Admissable 
heuristics never overestimate the cost to reach the goal, allowing 
admissible heuristics to find the shortest path. This program features 
both inadmissible and admissible heuristics, and the summary statistics 
printed during the program's runtime will indicate whether an algorithm 
is using an inadmissible or admissible heuristic, as well as a brief 
summary of the heuristic itself. BFS and DFID are exhaustive search 
techniques, and as a result do not use heuristics at all.

**Program Usage**
To run our program, a user must provide the start position of the puzzle.


This can be specified in a puzzle file, within the Lisp interpreter by 
passing a list to the 8puzzle function call, or interactively by calling 
the 8puzzle function without a start state list.

The puzzlefile contains an 8-puzzle start position, consisting of 9 digits 
separated by white space, in row-major order. The digits 1-8 represent 
the 8 tiles, and 0 represents the blank.

An example of a goal state is the following puzzle configuration:

1 2 3       
8 0 4 
7 6 5

This configuration will be represented in the CLISP program as the 
following list:

( 1 2 3 8 0 4 7 6 5 )

------------------------------------------------------------

**Program Usage Example: Running Program From Command Line**

easy.puz file:
1 3 4 
8 6 2 
7 0 5

Command Line: clisp 8puzzle.lsp puzzlefile

------------------------------------------------------------

**Program Usage Example: CLISP, passing start state as list**
( load '8puzzle )
( 8puzzle '( 1 3 4 8 6 2 7 0 5 ) )

------------------------------------------------------------

**Program Usage Example: CLISP without passing in start state**
( load '8puzzle )
( 8puzzle )
Please enter a puzzle:
>>1 3 4 8 6 2 7 0 5

------------------------------------------------------------

**Authors**
J. Anthony Brackins, Scott Carda, Leif Torgersen

**Course**
Written Spring 2016 for CSC447/547 AI class.

**Modifications**
For Additional Credit, the program has been expanded beyond the 
standard 8-puzzle to handle N-puzzles, where N may be:
(3^2) - 1 = 8 (standard 8-puzzle)
(4^2) - 1 = 15-puzzle
(5^2) - 1 = 24-puzzle, etc.

The program has been scaled up so that the program will be able to 
generate the goal state of any given puzzle and determine the puzzle 
size based on the size of the list read in as the initial puzzle state,
as long as the initial puzzle given to the program is in a valid 
N-puzzle format.

|#


#|--------------------------------------------------------------------------|#
#|                               Files Loaded                               |#
#|--------------------------------------------------------------------------|#

( load 'bfs )
( load 'a_star )
( load 'newdfid )
( load 'search-funcs )
( load 'read-puzzle )
( load 'print-puzzle )

#|--------------------------------------------------------------------------|#
#|                             8 Puzzle Routine                             |#
#|--------------------------------------------------------------------------|#

; Solves the passed in n-puzzle with several state-based search
; algorithms. Prompts the user for a puzzle if none is given.
( defun 8puzzle ( &optional ( puzzlelist nil ) )
    "Solves an n-puzzle using several state-space search algorithms."
    ( let 
        ( 
            ( puzzles_per_row 4 ) ; Number of puzzles printed in a row to the screen
            ( goal nil ) ; Goal state for the given puzzle's length
            ( n nil ) ; One less than the length of the puzzle, the 'n' of n-puzzle
;            ok ; Flag for if the puzzle is solvable
            solution ; Anser returned by an algorithm
        )
    
        ; If n > 8 just flag as ok, since
        ; solvable func doesnt work for non
        ; 8puzzles
        ( when ( null puzzlelist )
            ( format t "~%Please enter a puzzle:~%>>" )
            ( setf puzzlelist ( read-puzzle ) )
        )

        ( setf n ( - ( length puzzlelist ) 1 ) )

;        ( cond 
;            ; If n > 8 just flag as ok, since
;            ; solvable func doesnt work for non
;            ; 8puzzles
;            ( ( > n 8 )
;                ( setf ok t )
;            )
;
;            ; If we're dealing with an 8 puzzle,
;            ; see if it's solvable
;            ( ( solvable puzzlelist )
;                ( setf ok t )
;            )
;
;            ; If it's not... then set the flag
;            ( t
;                ( setf ok nil )
;            )
;        )
;    
;        ; If the program has passed "solvable" or 
;        ; if n > 8, then continue with running the program
;        ( cond
;            ( ( not ( null ok ) )
        ( cond

            ; If puzzle is blank
            ( ( not puzzlelist )
                ( format t "Error: Entered puzzle is blank.~%" )
            )
            
            ; If puzzle size is not a perfect square
            ( ( /=
                ( length puzzlelist )
                ( * ( isqrt ( length puzzlelist ) ) ( isqrt ( length puzzlelist ) ) )
              )
              ( format t "Error: Puzzle size is not a perfect square.~%" )
            )

            ; If the puzzle entered is not a solvable puzzle, prints message to the screen
            ( ( not ( solvablep puzzlelist ) )
                ( format t "The entered puzzle is not solvable.~%" )
            )

            ; If the puzzle entered is a solvable puzzle, use algorithms to solve it
            ( t
                ; Generate goal state for the algorithms
                ( setf goal ( generate-goal ( - ( length puzzlelist ) 1 ) ) )
            
                ; BFS
                ( setf solution ( bfs puzzlelist ) )
                ( print_stats solution "BFS" )
                ( print_puzzle solution n puzzles_per_row )

                ; DFID*
                ; Add DFID Solution steps here, and then print
                ( setf solution ( dfs  puzzlelist goal ) )
                ( print_stats solution "DFID" )
                ( print_puzzle solution n puzzles_per_row )

                ; A* with Hamming ( admissible )
                ( setf solution ( a* puzzlelist
                    #'( lambda ( state ) ( goal? state goal ) )
                    #'successors
                    #'( lambda ( state ) ( count_wrong state goal ) )
                ) )
                ( print_stats solution "A*" "Count Incorrect Elements ( Admissible )" )
                ( print_puzzle solution n puzzles_per_row )
                
                ; A* with Manhattan ( admissible )
                ( setf solution ( a* puzzlelist
                    #'( lambda ( state ) ( goal? state goal ) )
                    #'successors
                    #'( lambda ( state ) ( count_wrong_w_rot state goal ) )
                ) )
                ( print_stats solution "A*" "Count Manhattan Distance of Incorrect Elements ( Admissible )" )
                ( print_puzzle solution n puzzles_per_row )
                
                ; A* ( inadmissible )
                ( setf solution ( a* puzzlelist
                    #'( lambda ( state ) ( goal? state goal ) )
                    #'successors
                    #'( lambda ( state ) ( count_wrong_w_nilsson_score state goal ) )
                ) )
                ( print_stats solution "A*" "Count Manhattan Distance of Incorrect Elements and add Nilsson sequence score ( Inadmissible )" )
                ( print_puzzle solution n puzzles_per_row )
            )
        )
        
        ; Suppress NIL on return
        ( values )
    )
)

#|--------------------------------------------------------------------------|#
#|                              MAIN FUNCTION                               |#
#|--------------------------------------------------------------------------|#

( defun main ()
    ; File present, so read in the puzzle from file
	( when ( = ( length *args* ) 1 )
	    ( 8puzzle ( read-puzzle-file ( car *args* ) ) )
    )
)

( main )

