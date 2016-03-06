( defun find_goal ( state1 state2 &optional state3 state4 )
	(let ( ( result ) )
 		( when  ( equal 'state1 '( 1 2 3 8 0 4 7 6 5 ) )
			 ( setf result state1 )
		)

		( when ( equal 'state2 '( 1 2 3 8 0 4 7 6 5 ) ) 
			( setf result state2 )
		)

		( when ( equal 'state3 '( 1 2 3 8 0 4 7 6 5 ) ) 
			( setf result state3 )
		)

		( when ( equal 'state4 '( 1 2 3 8 0 4 7 6 5 ) ) 
			( setf result state4 )
		)
	)
)
