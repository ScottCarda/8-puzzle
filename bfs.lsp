#|
                    ***** BFS.LSP *****

Routine for performing Breadth-First Search on the 8-puzzle

Author: J. Anthony Brackins
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#


#|--------------------------------------------------------------------------|#
#|                               Files Loaded                               |#
#|--------------------------------------------------------------------------|#

; File that specifies the goal? function and
; the successors function required by the algorithm.
( load 'search-funcs )


#|--------------------------------------------------------------------------|#
#|                                Structs                                   |#
#|--------------------------------------------------------------------------|# 

; Initialize a Node Structure
(defstruct node state parent)

#|--------------------------------------------------------------------------|#
#|                              BFS Functions                               |#
#|--------------------------------------------------------------------------|#
( defun bfs ( puz_state )
    (do*
        (
            (current ( make-node :state puz_state :parent nil))
            ( open_list (list current) )
            (closed_list nil)
            ( succ_lst nil )
            ( return_list nil )
            ( new_node nil )
        )

        ( ( goal? ( node-state ( car open_list )) ) (setf goal_node (  car open_list ) )
            ;if this is true, then we've reached the solution
            ;reformat the answer.
            ;;( reformat_node best )

        )

        ;when the open_list is empty you're done!        
        ( when (null open_list ) (return nil))


        ; get current node from OPEN, update OPEN and CLOSED
        (setf current (car open_list))
        (setf open_list (cdr open_list))
        (setf closed_list (cons current closed_list))


        ; add successors of current node to OPEN
        ( setf succ_lst (successors (node-state current) ) )
            ; for each child node
        ( loop for s in succ_lst do 

            ; if the node is not on OPEN or CLOSED
            ; add to the end of the list
            (if (and
                  (not (member s open_list  ))
                  (not (member s closed_list))
                )
                (setf open_list (append open_list (list (make-node :state s :parent current))))
            )
        )
    )
)