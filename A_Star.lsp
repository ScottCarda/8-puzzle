( load 'search-funcs )

( defun A* ( state )
	( A*_search ( list 0 ( heuristic state ) state ) () )
)

( defun A*_search ( open_list closed_list )
	( cond
		( ( goalp 
	)	
)

( defun heuristic ( state )
	1
)

( find_best ( open_list &optional ( best () ) )
	( cond
		( ( not ( car open_list ) ) best )
		( ( not best ) ( find_best ( cdr open_list )( car open_list ) ) )
		( ( < ( car open_list ) best ) ( find_best ( cdr open_list ) ( car open_list ) ) )
		( t ( find_best ( cdr open_list ) best ) )
	)
)
