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
    ( let 
        ( 
            ( puzzles_per_row 4 )
            ( n nil )
            ok

            bfs_answer
            dfid_answer
            a_star_answer
        )
    

        ( cond 
            ;If n > 8 just flag as ok, since
            ;solvable func doesnt work for non
            ;8puzzles
            ( ( null puzzlelist )
                ( format t "~%Please enter a puzzle:~%>>" )
                ( 8puzzle ( read-puzzle ) )
            )
        )

        ( setf n ( - ( length puzzlelist ) 1 ) )

        ( cond 
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
        )

    
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
                ( setf dfid_answer ( dfid  puzzlelist ) )
                ( print_stats dfid_answer '"DFID" )
                ( print_puzzle dfid_answer n puzzles_per_row )

                ;A*
                ( setf a_star_answer ( a* puzzlelist #'goal? #'successors #'heuristic ) )
                ( print_stats a_star_answer '"A*" '"heuristic-name" )
                ( print_puzzle a_star_answer n puzzles_per_row )
            )
        )
    )
)


#|--------------------------------------------------------------------------|#
#|                       Read In Puzzle from CLI                            |#
#|--------------------------------------------------------------------------|#

( defun read-puzzle ()
    ( let ( ( str ( read-line ) ) )
        ( with-input-from-string ( stream str )
            ( get-puzzle stream )
        )   
    )
)

#|--------------------------------------------------------------------------|#
#|                      Read In Puzzle from File                            |#
#|--------------------------------------------------------------------------|#

( defun read-puzzle-file ( filename )
    ( let ( ( file ( open filename ) ) puzzlelist )
        ( when file
            ( setf puzzlelist ( get-puzzle file ) )
            ( close file )
            puzzlelist
        )
    )
)

( defun get-puzzle ( file )
    ( let ( ( input ( read file NIL NIL ) ) )
        ( cond
            ( ( not input )
                NIL
            )

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

