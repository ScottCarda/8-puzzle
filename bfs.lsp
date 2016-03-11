#|
                    ***** BFS.LSP *****

Routine for performing Breadth-First Search on the 8-puzzle

Author: J. Anthony Brackins
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#

;Set up similar to Scott's structure for A* file
( defun bfs ( state ) 
    ( cdr ( bfs_search ( list ( make_node 0 state NIL ) ) NIL ) )
)

; Initialize a Node Structure
(defstruct node state parent)


(defun bfs_search ( open_list closed_list )
    (let
        (
            best
            ;( best ( find_best open_list ) )
            current
            succ_g
            succ_lst
            return_list
        )
        ( setf succ_g ( 1+ ( car best ) ) )
        ( cond
            ( ( goal? ( caddr best ) )
                ;if this is true, then we've reached the solution
                ;reformat the answer.
                ;;( reformat_node best )
                1
            )
            ( t
                ; get current node from OPEN, update OPEN and CLOSED
                (setf current (car open_list))
                (setf open_list (cdr open_list))
                (setf closed_list (cons current closed_list))


                ; add successors of current node to OPEN
                    ; for each child node
                    ; if the node is not on OPEN or CLOSED
                        ; BFS - add to end of OPEN list (queue)
            )
        )
    )   
)



#| OLD ISH.
( defun old_bfs_search (start)
    (do*                                                    ; note use of sequential DO*
        (                                                   ; initialize local loop vars
            (curNode (make-node :state start :parent nil))  ; current node: (start nil)
            (OPEN (list curNode))                           ; OPEN list:    ((start nil))
            (CLOSED nil)                                    ; CLOSED list:  ( )
        )

        ; termination condition - return solution path when goal is found
        ;((goal? (node-state curNode)) (build-solution curNode CLOSED))

        ; loop body
        (when (null OPEN) (return nil))             ; no solution

        ; get current node from OPEN, update OPEN and CLOSED
        (setf curNode (car OPEN))
        (setf OPEN (cdr OPEN))
        (setf CLOSED (cons curNode CLOSED))

        ; add successors of current node to OPEN
        (dolist (child (generate-successors (node-state curNode)))

            ; for each child node
            (setf child (make-node :state child :parent (node-state curNode)))

            ; if the node is not on OPEN or CLOSED
            (if (and (not (member child OPEN   :test #'equal-states))
                     (not (member child CLOSED :test #'equal-states)))


                    ; BFS - add to end of OPEN list (queue)
                    (setf OPEN (append OPEN (list child)))

                
            )
        )
    )
)

|#