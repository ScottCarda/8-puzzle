
;--------------------------------------------------------------------------

; Node structure: stores state and parent.
(defstruct node state parent)

; Test if two nodes have the same state.
(defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))

;--------------------------------------------------------------------------

; Depth-first-search implements the OPEN list as a STACK of (state parent) nodes.
(defun dfs (start) 
	(let ((search_depth 0))
		(loop while ( not (search_dfs start 'dfs search_depth)) do
			(incf search_depth)
		)
	)
)

; Given a start state and a search type (BFS or DFS), return a path from the start to the goal.
(defun search_dfs (start type search_depth)
    (do*                                                    ; note use of sequential DO*
        (                                                   ; initialize local loop vars
            (curNode (make-node :state start :parent nil))  ; current node: (start nil)
            (OPEN (list curNode))                           ; OPEN list:    ((start nil))
            (CLOSED nil)                                    ; CLOSED list:  ( )
	    (current_depth 0)
        )

        ; termination condition - return solution path when goal is found
        ((goal? (node-state curNode)) (build-solution curNode CLOSED))

        ; loop body
        (when (null OPEN) (return nil))             ; no solution

	(when (> (+ current_depth 1 ) search_Depth) (return nil))

	(when (goal? (node-state curNode)) (return (build-solution curNode CLOSED)))

	(format t "~A~%" (node-state curNode))

	( incf current_depth )

        ; get current node from OPEN, update OPEN and CLOSED
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
        (setf CLOSED (cons curNode CLOSED))

        ; add successors of current node to OPEN
        (dolist (child (successors (node-state curNode)))

            ; for each child node
            (setf child (make-node :state child :parent (node-state curNode)))

            ; if the node is not on OPEN or CLOSED
            (if (and (not (member child OPEN   :test #'equal-states))
                     (not (member child CLOSED :test #'equal-states)))

                ; add it to the OPEN list
                (cond

                    ; DFS - add to start of OPEN list (stack)
                    ((eq type 'dfs) (setf OPEN (cons child OPEN)))

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
(defun build-solution (node node-list)
    (do
        ((path (list (node-state node))))        ; local loop var
        ((null (node-parent node)) path)         ; termination condition

        ; find the parent of the current node
        (setf node (member-state (node-parent node) node-list))

        ; add it to the path
        (setf path (cons (node-state node) path))
    )
)

; Member-state looks for a node on the node-list with the same state.
(defun member-state (state node-list)
    (dolist (node node-list)
        (when (equal state (node-state node)) (return node))
    )
)
