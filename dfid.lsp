#|
                    ***** DFID.LSP *****

Routine which performs a depth first iterated deepening
search on the state space for the 8 puzzle to fin an optimal 
solution.

Author: Leif Torgersen
Written Spring 2016 for CSC447/547 AI class.

|#

( defparameter *generated* 0 )
( defparameter *distinct* 0 )
( defparameter *expanded* 0 )
( defparameter *pathlength* 0 )

#|--------------------------------------------------------------------------|#
#|                               Files Loaded                               |#
#|--------------------------------------------------------------------------|#

;This file contails the goal? function used in this file
( load 'search-funcs )

#|--------------------------------------------------------------------------|#
#|                       Recursive DFS with make depth                      |#
#|--------------------------------------------------------------------------|#

( defun deepSearch ( currentState maxDepth currentDepth )
	( let ( goalFound )
		( cond
			;first basis case if the goal is found
			( ( goal? currentState )
				( list currentState )
			)
			;second basis case if we are as deep as we are 
			;allowed to go
			( ( >= ( + currentDepth 1 ) maxDepth )
				nil
			)
			;recursion if pervious 2 not done
			( t
				;counts if currentState is expanded
				( incf *expanded* )
				( dolist ( succ ( successors currentState ) )
					( when ( not goalFound )
						;counts each child generated
						( incf *generated* )
						;checks if child is distinct
						( cond
							( ( member succ goalFound )
								nil
							)
							( t
							;counts each child generated that is distinct
								( incf *distinct* )
							)
						)
						;actual recursive call, storing the return value
						( setf goalFound ( deepSearch succ maxDepth ( 1+ currentDepth ) ) )
					)
				)
				;This builds the path as we recurse out if the goal is found
				( if goalFound
					( cons currentState goalFound )
					nil
				)
			)
		)
	)
)

#|--------------------------------------------------------------------------|#
#|                    Iteration for iterated deepening                      |#
#|--------------------------------------------------------------------------|#

( defun dfid ( startState )
	( let ( ( searchDepth 0 ) pathReturn )
		;while loop which continues to increase depth and call dfid
		;if the goal is not yet found
		( loop while ( not pathReturn ) do
			( setf pathReturn ( deepSearch startState searchDepth 0 ) )
			( incf searchDepth )
		)
		;captures final path length
		( setf *pathlength* ( - ( length pathreturn ) 1 ) )
		pathReturn
	)
)
