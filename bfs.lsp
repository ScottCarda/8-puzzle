#|
                    ***** BFS.LSP *****

Routine for performing Breadth-First Search on the 8-puzzle

Author: J. Anthony Brackins
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#

( load 'search-funcs )

;Set up similar to Scott's structure for A* file
( defun bfs ( state ) 
    ( cdr ( bfs_search ( list ( make_node 0 state NIL ) ) NIL ) )
)

; Initialize a Node Structure
(defstruct node state parent)


(defun bfs_search ( state )
    (do*

        (
            (current state)
            ( open_list (list state) )
            (closed_list nil)
            ( succ_lst nil )
            ( return_list nil )
        )

        ( ( goal? ( car open_list ) ) (print (car open_list ))
            ;if this is true, then we've reached the solution
            ;reformat the answer.
            ;;( reformat_node best )

        )
        
        ( when (null open_list ) (return nil))
            ; get current node from OPEN, update OPEN and CLOSED
            (setf current (car open_list))
            (setf open_list (cdr open_list))
            (setf closed_list (cons current closed_list))


            ; add successors of current node to OPEN
            ( setf succ_lst (successors current) )
                ; for each child node
            ( loop for s in succ_lst do 
                (print s)

                ; if the node is not on OPEN or CLOSED
                ; add to the end of the list
                (if (and
                      (not (member s open_list  ))
                      (not (member s closed_list))
                    )
                    (setf open_list (append open_list (list s)))
                )
            )

    )
)