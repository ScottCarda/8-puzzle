( defun deepSearch ( currentState maxDepth currentDepth )
	( let ( goalFound )
		( cond
			( ( goal? currentState )
				( list currentState )
			)
			( ( >= ( + currentDepth 1 ) maxDepth )
				nil
			)
			( t
				( dolist ( succ ( successors currentState ) )
					( when ( not goalFound )
						( setf goalFound ( deepSearch succ maxDepth ( 1+ currentDepth ) ) )
					)
				)
				( if goalFound
					( cons currentState goalFound )
					nil
				)
			)
		)
	)
)

( defun dfid ( startState )
	( let ( ( searchDepth 0 ) pathReturn )
		( loop while ( not pathReturn ) do
			( setf pathReturn ( deepSearch startState searchDepth 0 ) )
			( incf searchDepth )
		) 
		pathReturn
	)
)
