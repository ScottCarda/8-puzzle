#|
                    ***** 8PUZZLE.LSP *****

8 Puzzle program for Artificial Intelligence.

Authors: J. Anthony Brackins, Scott Carda, Leif Torgersen
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#


#|--------------------------------------------------------------------------|#
#|                               Files Loaded                               |#
#|--------------------------------------------------------------------------|#

( load 'bfs )
( load 'a_star )
( load 'dfid )
( load 'search-funcs )
( load 'print_puzzle )
( load 'solvable     )

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
            ok ; Flag for if the puzzle is solvable
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

        ( cond 
            ; If n > 8 just flag as ok, since
            ; solvable func doesnt work for non
            ; 8puzzles
            ( ( > n 8 )
                ( setf ok t )
            )

            ; If we're dealing with an 8 puzzle,
            ; see if it's solvable
            ( ( solvable puzzlelist )
                ( setf ok t )
            )

            ; If it's not... then set the flag
            ( t
                ( setf ok nil )
            )
        )
    
        ; If the program has passed "solvable" or 
        ; if n > 8, then continue with running the program
        ( cond
            ( ( not ( null ok ) )
            
                ; Generate goal state for the algorithms
                ( setf goal ( generate-goal ( - ( length puzzlelist ) 1 ) ) )
            
                ; BFS
                ( setf solution ( bfs puzzlelist ) )
                ( print_stats solution '"BFS" )
                ( print_puzzle solution n puzzles_per_row )

                ; DFID*
                ; Add DFID Solution steps here, and then print
                ( setf solution ( dfid  puzzlelist (- ( length puzzlelist ) 1) ) )
                ( print_stats solution '"DFID" )
                ( print_puzzle solution n puzzles_per_row )

                ; A* with Hamming ( admissible )
                ( setf solution ( a* puzzlelist
                    #'( lambda ( state ) ( goal? state goal ) )
                    #'successors
                    #'( lambda ( state ) ( count_wrong state goal ) )
                ) )
                ( print_stats solution '"A*" '"Count Incorrect Elements ( Admissible )" )
                ( print_puzzle solution n puzzles_per_row )
                
                ; A* with Manhattan ( admissible )
                ( setf solution ( a* puzzlelist
                    #'( lambda ( state ) ( goal? state goal ) )
                    #'successors
                    #'( lambda ( state ) ( count_wrong_w_rot state goal ) )
                ) )
                ( print_stats solution '"A*" '"Count Manhattan Distance of Incorrect Elements ( Admissible )" )
                ( print_puzzle solution n puzzles_per_row )
                
                ; A* ( inadmissible )
                ( setf solution ( a* puzzlelist
                    #'( lambda ( state ) ( goal? state goal ) )
                    #'successors
                    #'( lambda ( state ) ( count_wrong_w_nilsson_score state goal ) )
                ) )
                ( print_stats solution '"A*" '"Count Manhattan Distance of Incorrect Elements and add Nilsson sequence score ( Inadmissible )" )
                ( print_puzzle solution n puzzles_per_row )
            )
            
            ; If the puzzle entered is not a solvable puzzle, prints message to the screen
            ( t
                ( format t "The entered puzzle is not solvable.~%" )
            )
        )
        
        ; Suppress NIL on return
        ( values )
    )
)


#|--------------------------------------------------------------------------|#
#|                       Read In Puzzle from CLI                            |#
#|--------------------------------------------------------------------------|#

; Gets a puzzle from user input.
( defun read-puzzle ()
    "Gets a puzzle from user input."
    ( let
		(
			; The user input as a string
			( str ( read-line ) )
		)
		; Pretend the string is a file and pass it to the 
        ( with-input-from-string ( stream str )
            ( get-puzzle stream )
        )
    )
)

#|--------------------------------------------------------------------------|#
#|                      Read In Puzzle from File                            |#
#|--------------------------------------------------------------------------|#

; Reads a puzzle in from a file.
( defun read-puzzle-file ( filename )
    "Reads a puzzle from a file."
    ( let
		(
			( file ( open filename ) ) ; The file stream
			puzzlelist	; The puzzle list to be returned
		)

		; If the file was successfully opened
        ( when file
			; Uses the stream-reading function to get the puzzle list
            ( setf puzzlelist ( get-puzzle file ) )
            ( close file )
			; Returns the puzzle list
            puzzlelist
        )
    )
)

; Recursively reads a puzzle from an input stream.
( defun get-puzzle ( file )
    "Reads a puzzle from an input stream."
    ( let
		(
			; Reads the next input from the file
			( input ( read file NIL NIL ) )
		)

        ( cond
			; If there is no more input ( base case )
            ( ( not input )
                NIL
            )

			; Else recurses
            ( t
                ( cons input ( get-puzzle file ) )
            )
        )
    )
)

#|--------------------------------------------------------------------------|#
#|                              MAIN FUNCTION                               |#
#|--------------------------------------------------------------------------|#

( defun main ()
    ;File present, so read in the puzzle from file
	( when ( = ( length *args* ) 1 )
	    ( 8puzzle ( read-puzzle-file ( car *args* ) ) )
    )
)

( main )

