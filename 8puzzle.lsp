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
( load 'dfid)
( load 'mapper       )
( load 'search-funcs )
( load 'print_puzzle )
( load 'solvable     )

#|--------------------------------------------------------------------------|#
#|                             8 Puzzle Routine                             |#
#|--------------------------------------------------------------------------|#

( defun 8puzzle ( &optional ( puzzlelist nil ) )
    "This is a docstring."
    ( let 
        ( 
            ( puzzles_per_row 4 )
            ( goal nil )
            ( n nil )
            ok

            bfs_answer
            dfid_answer
            a_star_answer
        )
    
        ;If n > 8 just flag as ok, since
        ;solvable func doesnt work for non
        ;8puzzles
        ( when ( null puzzlelist )
                ( format t "~%Please enter a puzzle:~%>>" )
                ( setf puzzlelist ( read-puzzle ) )
        )

        ( setf n ( - ( length puzzlelist ) 1 ) )

        #|( cond 
            ;If n > 8 just flag as ok, since
            ;solvable func doesnt work for non
            ;8puzzles
            ( ( > n 8 )
                ( setf ok t )
            )

            ;If we're dealing with an 8 puzzle,
            ;see if it's solvable
            ( ( solvable puzzlelist )
                ( setf ok t )
            )

            ;if it's not... then set the flag
            ( t
                ( setf ok nil )
            )
        )|#
        
        ( setf ok T )

    
        ;If the program has passed "solvable" or 
        ;if n > 8, then continue with running the program
        ( cond
            ( ( not ( null ok ) ) 
                ;BFS
                ( setf bfs_answer ( bfs puzzlelist ) )
                ( print_stats bfs_answer '"BFS" )
                ( print_puzzle bfs_answer n puzzles_per_row )


                ;DFID*
                ;Add DFID Solution steps here, and then print
                ( setf dfid_answer ( dfid  puzzlelist (- ( length puzzlelist ) 1) ) )
                ( print_stats dfid_answer '"DFID" )
                ( print_puzzle dfid_answer n puzzles_per_row )

                ;Generate goal state for a* function arguments
                ( setf goal ( generate_goal ( - ( length puzzlelist ) 1 ) ) )

                ;A*
                ( setf a_star_answer ( a* puzzlelist
                    #'( lambda ( state ) ( goal? state goal ) )
                    #'successors
                    #'( lambda ( state ) ( count_wrong state goal ) )
                ) )
                ( print_stats a_star_answer '"A*" '"Count Incorrect Elements" )
                ( print_puzzle a_star_answer n puzzles_per_row )
            )
        )
        
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

