( load 'search-funcs )

( defun A* ( state )
	( reverse
		( cddr ( A*_search ( list ( make_node 0 state NIL ) ) () ) )
	)
)

( defun A*_search ( open_list closed_list )
	( let
		(
			( best ( find_best open_list ) )
			( succ_g ( 1+ ( car best ) ) )
			succ_lst
		)
		( cond
			( ( goal? ( caddr best ) ) best )
			( t
				( mov_elem_between_lsts best open_list closed_list )
				( setf succ_list ( map #'( lambda ( state ) ( make_node succ_g state best ) ) ( successors ( caddr best ) ) ) )
				( process_succs succ_list open_list closed_list )
				( A*_search open_list closed_list )
			)
		)
	)
)

( defun make_node ( g_val state parent)
	( append ( list g_val ( heuristic state ) state ) ( cddr parent ) )
)

( defun heuristic ( state )
	1
)

( defun find_best ( open_list &optional ( best () ) )
	( cond
		( ( not ( car open_list ) ) best )
		( ( not best ) ( find_best ( cdr open_list )( car open_list ) ) )
		( ( < ( get_f_val ( car open_list ) ) ( get_f_val best ) )
			( find_best ( cdr open_list ) ( car open_list ) )
		)
		( t ( find_best ( cdr open_list ) best ) )
	)
)

( defun get_f_val ( node )
	( + ( car node ) ( cadr node ) )
)

( defun mov_elem_between_lsts ( elem a_list b_list )
	( when ( member elem a_list :test #'equal )
		( delete elem a_list :test #'equal )
		( nconc b_list ( list elem ) )
	)
)

( defun process_succs ( succ_list open_list closed_list )
	( let
		(
			( succ ( car succ_list ) )
			extra
		)
		( unless ( not succ ) 
			( setf extra ( car ( member succ closed_list :test #'state_equal ) ) )
			( cond
				( ( not extra )
					( setf extra ( car ( member succ open_list :test #'state_equal ) ) )
					( if ( not extra )
						( nconc open_list ( list succ ) )
						( when ( < ( get_f_val succ ) ( get_f_val extra ) )
							( delete extra open_list )
							( nconc open_list ( list succ ) )
						)
					)
				)
				( t
					( when ( < ( get_f_val succ ) ( get_f_val extra ) )
						( delete extra closed_list )
						( nconc open_list ( list succ ) )
					)
				)
			)
			( process_succs ( cdr succ_list ) open_list cloased_list )
		)
	)
)

( defun state_equal ( node1 node2 )
	( equal ( caddr node1 ) ( caddr node2 ) )
)
