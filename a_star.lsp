( load 'search-funcs )

( defun A* ( state )
	;( reverse
	;	( cddr ( A*_search ( list ( make_node 0 state NIL ) ) NIL ) )
	;)
	( cdr ( A*_search ( list ( make_node 0 state NIL ) ) NIL ) )
)

( let ( ( inc 0 ) )
	( defun test ()
		( format t "~D~%" inc )
		( setf inc ( 1+ inc ) )
	)
)

( defun test_list ()
	'( ( 1 0 ( 1 2 3 4 5 6 7 8 0 ) ) ( 1 1 ( 0 8 7 6 5 4 3 2 1 ) ) )
)

( defun A*_search ( open_list closed_list )
	;( format t "A*_search called with parameters:~%" )
	;( format t "    open_list: ~A~%" open_list )
	;( format t "    closed_list: ~A~%" closed_list )
	;( format t "Continue?(Y|N): " )
	;( let ( c )
	;	( setf c ( read ) )
	;	( unless ( eq c 'N )
	( let
		(
			( best ( find_best open_list ) )
			both
			succ_g
			succ_lst
			return_list
		)
		( setf succ_g ( 1+ ( car best ) ) )
		( cond
			( ( goal? ( caddr best ) )
				;( format t "Base Case Return: ~A -> ~A~%" best ( reformat_node best ) )
				( reformat_node best )
			)
			( t
				( setf both ( mov_elem_between_lsts best open_list closed_list ) )
				( setf open_list ( car both ) )
				( setf closed_list ( cadr both ) )
				( setf succ_list ( map 'list #'( lambda ( state ) ( make_node succ_g state best ) ) ( successors ( caddr best ) ) ) )
				( setf both ( process_succs succ_list open_list closed_list ) )
				( setf open_list ( car both ) )
				( setf closed_list ( cadr both ) )
				
				( setf return_list ( A*_search open_list closed_list ) )
				
				( when ( car return_list )
					( setf  return_list ( cons ( nth 3
						( get_node_with_state
							( car return_list )
							( append open_list closed_list )
						)
					) return_list ) )
				)
				return_list
			)
		)
	)
	;	)
	;)
)

( defun get_node_with_state ( state node_list )
	( car ( member
		state
		node_list
		:test #'( lambda ( state node ) ( equal state ( caddr node ) ) )
	))
)

( defun reformat_node ( node )
	( list ( nth 3 node ) ( nth 2 node ) )
)

;( defun make_node ( g_val state parent)
;	( append ( list g_val ( heuristic state ) state ) ( cddr parent ) )
;)

( defun make_node ( g_val state parent)
	( list g_val ( heuristic state ) state ( caddr parent ) )
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
	( let ( both )
	( when ( member elem a_list :test #'equal )
	;	( format t "HERE!" )
		( setf a_list ( remove elem a_list :test #'equal ) )
		( setf b_list ( cons elem b_list ) )
		( setf both ( list a_list b_list ) )
	)
	;( format t "Returning from mov~%" )
	;( format t "    Elem: ~A~%" elem )
	;( format t "    Source-List: ~A~%" a_list )
	;( format t "    Dest-List: ~A~%~%" b_list )
	both
	)
)

; This needs a rewrite
( defun process_succs ( succ_list open_list closed_list )
	;( format t "Processing Successor: ~A~%" ( car succ_list ) )
	;( format t "Processing Open: ~A~%" open_list )
	;( format t "Processing Closed: ~A~%~%" closed_list )
	( let
		(
			( succ ( car succ_list ) )
			( both NIL )
			extra
		)
		( cond
			( ( not succ )
				( list open_list closed_list )
			)
			( t
				;( setf extra ( car ( member succ closed_list :test #'state_equal ) ) )
				;( cond
				;	( ( not extra )
				;		( setf extra ( car ( member succ open_list :test #'state_equal ) ) )
				;		( if ( not extra )
				;			( nconc open_list ( list succ ) )
				;			( when ( < ( get_f_val succ ) ( get_f_val extra ) )
				;				( delete extra open_list )
				;				( nconc open_list ( list succ ) )
				;			)
				;		)
				;	)
				;	( t
				;		( when ( < ( get_f_val succ ) ( get_f_val extra ) )
				;			( delete extra closed_list )
				;			( nconc open_list ( list succ ) )
				;		)
				;	)
				;)
	
				( cond
					( ( setf extra ( car ( member succ closed_list :test #'state_equal ) ) )
						( when ( < ( get_f_val succ ) ( get_f_val extra ) )
							( setf closed_list ( remove extra closed_list ) )
							( setf open_list ( cons succ open_list ) )
						)
					)
					( ( setf extra ( car ( member succ open_list :test #'state_equal ) ) )
						( when ( < ( get_f_val succ ) ( get_f_val extra ) )
							( setf open_list ( remove extra open_list ) )
							( setf open_list ( cons succ open_list ) )
						)
					)
					( t
						( setf open_list ( cons succ open_list ) )
					)
				)

				( process_succs ( cdr succ_list ) open_list closed_list )
			)
		)
	)
)

( defun state_equal ( node1 node2 )
	( equal ( caddr node1 ) ( caddr node2 ) )
)
