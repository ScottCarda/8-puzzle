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
;( load '"DepthFirstID")
( load 'mapper )
( load 'search-funcs )
( load 'print_puzzle )

#|--------------------------------------------------------------------------|#
#|                             8 Puzzle Routine                             |#
#|--------------------------------------------------------------------------|#

( defun 8puzzle ( &optional puzzlelist )
    ( cond
        
        ( ( = ( length puzzlelist ) 0 )
        
            ; Replace this with the search algorithm to be run
            ;( printState ( read-puzzle ) )
            ( print_puzzles ( a* ( read-puzzle ) #'goal? #'successors #'heuristic ) ( - ( length puzzlelist ) 1 ) 4 )
            
        )
        
    		
        ( t



            ;BFS
            ( setf bfs_answer ( bfs puzzlelist ) )
            ( print_stats bfs_answer '"BFS" )
            ( print_puzzle bfs_answer ( - ( length puzzlelist ) 1) 4 )


            ;DFID*
            ;Add DFID Solution steps here, and then print
            ;( print_puzzle dfid_answer )

            ;A*
            ( setf a_star_answer ( a* puzzlelist #'goal? #'successors #'heuristic ) )
            ( print_stats a_star_answer '"A*" '"heuristic-name" )
            ( print_puzzle a_star_answer ( - ( length puzzlelist ) 1) 4 )



        )
    )
	
	; Suppress NIL
	( values )
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
	( cond 
        ;No arguments, so read in the puzzle from CLI
		( ( not ( = ( length *args* ) 1 ) )
            ( format t "~%Please enter a puzzle:~%>>" )
            ( 8puzzle ( read-puzzle ) )
		)

	)

    ;File present, so read in the puzzle from file
	( when ( = ( length *args* ) 1 )
	    ( 8puzzle ( read-puzzle-file ( car *args* ) ) )
    )
)

( main )

