( defun 8puzzle ( &optional puzzlelist )
    ( cond
        
        ( ( = ( length puzzlelist ) 0 )
        
            ( format t "Please enter a puzzle:" )
            ; Replace this with the search algorithm to be run
            ( printState ( read-puzzle ) )
            
        )
        
        ( ( < ( length puzzlelist ) 9 )
        
            ( format t "Error: Incorrect puzzle size!" )
        )
    		
        ( t
            ; Replace this with the search algorithm to be run
            ( printState puzzlelist )
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

