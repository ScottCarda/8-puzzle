( defun 8puzzle ( puzzlelist )
    ( cond
        ( ( = ( length puzzlelist ) 0 )
            ( format t "No List! - puzzle" )
        )
    		
        ( t
            ( printState puzzlelist )
        )
	)
	
	; Suppress NIL
	( values )
)

( defun printState ( state )
    ( let (
            ( i 0 )
          )
        ( dolist ( elem state )
            ( if ( = ( mod i 3 ) 0 ) ( format t "~%" ) )
            ( format t "~D " elem )
            ( setf i ( 1+ i ) )
        )
    )
)

( defun main ()
	( cond 
		( ( not *args* )
			( format t "Usage statement!" )
		)

		( t 
			( 8puzzle *args* )
		)
	)
)

( main )

