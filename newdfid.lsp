#|
                    ***** DFID.LSP *****

Routine which performs a depth first iterated deepening
search on the state space for the n puzzle to find an optimal 
solution.

It takes in a start state, and optionally a goal state if an 
N-puzzle case, or otherwise assumes the 8 puzzle goal. Then 
It enters a loop which calls the depth first search with a
depth bound. This search is performed to that depth bound, 
or until a goal is found. If the bound is reached, then
the depth bound is increased and the search is repeated.
If the goal is reached, then the solution is build using the
parent values, and returned. This then activates the exit
condition of the calling function's loop. Lasting the goal
path is returned to the main function.

Author: Leif Torgersen
Written Spring 2016 for CSC447/547 AI class.

|#

( defparameter *generated* 0 )
( defparameter *distinct* 0 )
( defparameter *expanded* 0 )

#|--------------------------------------------------------------------------|#
#|                             Deepnode Struct                              |#
#|--------------------------------------------------------------------------|#

; Node structure: stores state and parent.
(defstruct deepnode state parent depth)

; Test if two nodes have the same state.
(defun deepequal-states (n1 n2)
	(equal (deepnode-state n1) (deepnode-state n2))
)

#|--------------------------------------------------------------------------|#
#|                               DFID Routines                              |#
#|--------------------------------------------------------------------------|#

; Depth-first-search implements the OPEN
; list as a STACK of (state parent) nodes.
(defun dfs (start &optional (goal '(1 2 3 8 0 4 7 6 5))) 
	"Calls DFS and iterates depth we search to"
	(let ((search_depth 0)(final_path))
		( setf *generated* 0 )
		( setf *distinct* 1 )
		( setf *expanded* 0 )
		; While loop which continues to increase depth and call DFID
		(loop while ( not final_path ) do
			(setf final_path (search_dfs start 'dfs search_depth goal))
			(incf search_depth)
		)
		final_path
	)
)

; Given a start state and a search type (BFS or DFS),
; return a path from the start to the goal.
(defun search_dfs (start type search_depth goal)
	"Performs DFS to the designated depth"
	(do* ; Note use of sequential DO*
		( ; Initialize local loop vars
			; Current node: (start nil)
			(curNode (make-deepnode :state start :parent nil :depth 0))
			(OPEN (list curNode)) ; OPEN list:    ((start nil))
			(CLOSED nil) ; CLOSED list:  ( )
		)

		; Termination condition - return solution path when goal is found
		((equal goal (deepnode-state curNode)) (build-solution curNode CLOSED))

		; Loop body
		(when (null OPEN) 
			(return nil)		; No solution
		)

		; Get current node from OPEN, update OPEN and CLOSED
		(setf curNode (car OPEN))
		(setf OPEN (cdr OPEN))
		(setf CLOSED (cons curNode CLOSED))

		; Add successors of current node to OPEN
		(dolist (child (successors (deepnode-state curNode)))

			; For each child node
			(setf child (make-deepnode
				:state child
				:parent (deepnode-state curNode)
				:depth ( + (deepnode-depth curNode) 1 )
			))

			(setf *generated* (1+ *generated*))

			(when (< (deepnode-depth child) (+ 1 search_depth))
				; If the node is not on OPEN or CLOSED
				(cond 

					(
						(and
							(not (member
								child
								OPEN
								:test #'deepequal-states
							))
							(not (member
								child
								CLOSED
								:test #'deepequal-states
							))
						)
						; DFS - add to start of OPEN list (stack)
						(setf OPEN (cons child OPEN))
						(setf *distinct* (1+ *distinct*))
						(when (< (deepnode-depth child) search_depth)
							(setf *expanded* (1+ *expanded*))
						)
					)
					; Else add it if the match has a larger depth
					(t
						(cond
							((member child OPEN   :test #'deepequal-states)
								(when
									(>
										(deepnode-depth (car (member
											child
											OPEN
											:test #'deepequal-states
										)))
										(deepnode-depth child)
									)

									(setf OPEN (cons child OPEN))
									(setf *distinct* (1+ *distinct*))

									(when
										(< 
											(deepnode-depth child)
											search_depth
										)

										(setf *expanded* (1+ *expanded*))
									)
								)
							)
							((member child CLOSED :test #'deepequal-states)
								(when
									(>
										(deepnode-depth (car (member
											child
											CLOSED
											:test #'deepequal-states
										)))
										(deepnode-depth child)
									)
									(setf OPEN (cons child OPEN))
									(setf *distinct* (1+ *distinct*))
									(when
										(<
											(deepnode-depth child)
											search_depth
										)
										(setf *expanded* (1+ *expanded*))
									)
								)
							)
						)
					)
				)
			)
		)
	)
)

#|--------------------------------------------------------------------------|#
#|                            Solution Routines                             |#
#|--------------------------------------------------------------------------|#

; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
(defun build-solution (deepnode deepnode-list)
	"Builds the solution once the goal is found"
	(do
		((path (list (deepnode-state deepnode))))		; Local loop var
		((null (deepnode-parent deepnode)) path)		; Termination condition

		; Find the parent of the current node
		(setf
			deepnode
			(deepmember-state (deepnode-parent deepnode) deepnode-list)
		)

		; Add it to the path
		(setf path (cons (deepnode-state deepnode) path))
	)
)

; The deepMember-state looks for a node on the node-list with the same state.
(defun deepmember-state (state deepnode-list)
	"Checks to see if a node is in the list"
	(dolist (deepnode deepnode-list)
		(when (equal state (deepnode-state deepnode)) (return deepnode))
	)
)
