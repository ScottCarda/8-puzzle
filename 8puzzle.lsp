
                    ***** 8PUZZLE.LSP *****

8 Puzzle program for Artificial Intelligence.

Authors: J. Anthony Brackins, Scott Carda, Leif Torgersen
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#



( load 'bfs )
;( load 'a_star )
;( load '"DepthFirstID")
( load 'mapper )
( load 'search-funcs )
( load 'print_puzzle )

( defun 8puzzle ( &optional puzzlelist )
    ( cond
        
        ( ( = ( length puzzlelist ) 0 )
        
            ( format t "Please enter a puzzle:" )
            ; Replace this with the search algorithm to be run
            ;( printState ( read-puzzle ) )
            ( print_puzzles ( a* ( read-puzzle ) #'goal? #'successors #'heuristic ) 4 )
            
        )
        
    		
        ( t
            ; Replace this with the search algorithm to be run
            ;( printState puzzlelist )
            ( print_puzzles ( a* puzzlelist #'goal? #'successors #'heuristic ) 4 )



            ;BFS
            ( setf bfs_answer ( bfs puzzlelist ) )
            ( print_puzzle bfs_answer )

            ;DFID*
            ;Add DFID Solution steps here, and then print
            ;( print_puzzle dfid_answer )

            ;A*
            ;Add A* Solution(s) here, and then print
            ; ( print_puzzle a_star_answer )

        )
    )
	
	; Suppress NIL
	( values )
)

( defun read-puzzle ()
    ( let ( ( str ( read-line ) ) )
        ( with-input-from-string ( stream str )
            ( get-puzzle stream )
        )   
    )
)

( defun printState ( state )
    ( let ( ( i 0 ) )
        ( dolist ( elem state )
            ( if ( = ( mod i 3 ) 0 ) ( format t "~%" ) )
            ( format t "~D " elem )
            ( setf i ( 1+ i ) )
        )
    )
    ( values )
)

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

( defun main ()
	;( cond 
	;	( ( not ( = ( length *args* ) 1 ) )
	;		( format t "Usage statement!" )
	;	)
    ;
	;	( t 
	;		( 8puzzle ( read-puzzle-file ( car *args* ) ) )
	;	)
	;)
	( when ( = ( length *args* ) 1 )
	    ( 8puzzle ( read-puzzle-file ( car *args* ) ) )
    )
)

( main )

