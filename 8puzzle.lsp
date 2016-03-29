#|
                    ***** 8PUZZLE.LSP *****

8 Puzzle program for Artificial Intelligence.

In the study of Artificial Intelligence, the 8-puzzle is a 
simple sliding puzzle "toy" problem used to illustrate the 
concepts of search space. To solve this puzzle, 8 tiles are 
repositioned about a 3x3 grid in a sliding fashion in order 
to acheive a goal state. A standard 8-puzzle game is 
simulated below:

    1 3 4       1 3 4       1 3 4       1 3         
    8 6 2   ->  8   2   ->  8 2     ->  8 2 4   ->  
    7   5       7 6 5       7 6 5       7 6 5       

    1   3       1 2 3       
    8 2 4   ->  8   4   <- (This is the goal state!)    
    7 6 5       7 6 5       

The objective of this assignment is to use the Lisp programming 
language to solve the 8-puzzle using Breadth-First Search (BFS), 
Depth First Iterated Deepening (DFID), and A*, a heuristics-based 
search method.

Usage:

To run our program, a user must provide the start position of the puzzle.


This can be specified in a puzzle file, within the Lisp interpreter by 
passing a list to the 8puzzle function call, or interactively by calling 
the 8puzzle function without a start state list.

The puzzlefile contains an 8-puzzle start position, consisting of 9 digits 
separated by white space, in row-major order. The digits 1-8 represent 
the 8 tiles, and 0 represents the blank.

------------------------------------------------------------

Example: easy.puz 
1 3 4 
8 6 2 
7 0 5

Command Line: clisp 8puzzle.lsp puzzlefile

------------------------------------------------------------

Example: Using CLISP and passing in start state as a list
( load '8puzzle )
( 8puzzle '( 1 3 4 8 6 2 7 0 5 ) )

------------------------------------------------------------

Example: Using CLISP without passing in start state
( load '8puzzle )
( 8puzzle )
Please enter a puzzle:
>>1 3 4 8 6 2 7 0 5

------------------------------------------------------------

Authors: J. Anthony Brackins, Scott Carda, Leif Torgersen
Written Spring 2016 for CSC447/547 AI class.

Modifications: 
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
( load 'dfid )
( load 'search-funcs )
( load 'read-puzzle )
( load 'print_puzzle )

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
                ( setf solution ( dfid  puzzlelist goal ) )
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

