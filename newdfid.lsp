
;--------------------------------------------------------------------------

; Node structure: stores state and parent.
(defstruct deepnode state parent depth)

; Test if two nodes have the same state.
(defun deepequal-states (n1 n2) (equal (deepnode-state n1) (deepnode-state n2)))

;--------------------------------------------------------------------------

; Depth-first-search implements the OPEN list as a STACK of (state parent) nodes.
(defun dfs (start &optional (goal '(1 2 3 8 0 4 7 6 5))) 
	(let ((search_depth 0)(final_path))
		(loop while ( not final_path ) do
			(setf final_path (search_dfs start 'dfs search_depth goal))
			(incf search_depth)
		)
		final_path
	)
)

; Given a start state and a search type (BFS or DFS), return a path from the start to the goal.
(defun search_dfs (start type search_depth goal)
    (do*                                                    		 ; note use of sequential DO*
        (                                                  		 ; initialize local loop vars
            (curNode (make-deepnode :state start :parent nil :depth 0))  ; current node: (start nil)
            (OPEN (list curNode))                       	         ; OPEN list:    ((start nil))
            (CLOSED nil)                                   		 ; CLOSED list:  ( )
        )

        ; termination condition - return solution path when goal is found
        ((equal goal (deepnode-state curNode)) (build-solution curNode CLOSED))

        ; loop body
        (when (null OPEN) (return nil))             ; no solution

        ; get current node from OPEN, update OPEN and CLOSED
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
        (setf CLOSED (cons curNode CLOSED))

        ; add successors of current node to OPEN
        (dolist (child (successors (deepnode-state curNode)))

            ; for each child node
            (setf child (make-deepnode :state child :parent (deepnode-state curNode) :depth ( + (deepnode-depth curNode) 1 )))

            ; if the node is not on OPEN or CLOSED
            (if (and (and (not (member child OPEN   :test #'deepequal-states))
                     (not (member child CLOSED :test #'deepequal-states))
		     (< (deepnode-depth child) (1+ search_depth) ) ) )

                ; add it to the OPEN list
                (cond

                    ; DFS - add to start of OPEN list (stack)
                    ( (eq type 'dfs) (setf OPEN (cons child OPEN)))

                    ; error handling for incorrect usage
                    (t (format t "SEARCH: bad search type! ~s~%" type) (return nil))
                )
            )
        )
    )
)

;--------------------------------------------------------------------------

; Build-solution takes a state and a list of (state parent) pairs
; and constructs the list of states that led to the current state
; by tracing back through the parents to the start node (nil parent).
(defun build-solution (deepnode deepnode-list)
    (do
        ((path (list (deepnode-state deepnode))))        ; local loop var
        ((null (deepnode-parent deepnode)) path)         ; termination condition

        ; find the parent of the current node
        (setf deepnode (deepmember-state (deepnode-parent deepnode) deepnode-list))

        ; add it to the path
        (setf path (cons (deepnode-state deepnode) path))
    )
)

; deepMember-state looks for a node on the node-list with the same state.
(defun deepmember-state (state deepnode-list)
    (dolist (deepnode deepnode-list)
        (when (equal state (deepnode-state deepnode)) (return deepnode))
    )
)
