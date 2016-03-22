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

#|--------------------------------------------------------------------------|#
#|                               Global Vars                                |#
#|--------------------------------------------------------------------------|#

( defparameter *generated* 0 )
( defparameter *distinct*  1 )
( defparameter *expanded*  0 )

#|--------------------------------------------------------------------------|#
#|                                Structs                                   |#
#|--------------------------------------------------------------------------|# 

; Initialize a Node Structure
( defstruct node state parent )

; Test if two nodes have the same state.
( defun equal-states (n1 n2) (equal (node-state n1) (node-state n2)))

( defun bfs ( puz_state )

    ;clear out global vars
    ( setf *generated* 0 )
    ( setf *distinct*  1 )
    ( setf *expanded*  0 )

    ;iterative bfs-search using do*
    ( bfs-search-do puz_state ( generate_goal ( - ( length puz_state ) 1) ) )

)

#|--------------------------------------------------------------------------|#
#|                              BFS Functions                               |#
#|--------------------------------------------------------------------------|#
( defun bfs-search-do ( puz_state g_state )
    (do*
        (
            (current ( make-node :state puz_state :parent nil ) )
            ( open_list (list current) )
            ( closed_list   nil )
            ( successor_lst nil )
            ( return_list   nil )
            ( new_node      nil )

            ( n_value ( - ( length puz_state ) 1) )
            ( node_count 0 )
            ( temp_node nil )
            ( goal_node nil )
            ( state_list nil )
        )

        ;DO* exit condition:
        ;Check the state of the node at the front of the open list.
        ;If this value is 
        ( ( goal? ( node-state ( car open_list ) ) g_state ) 

            ;This is our answer
            ( setf goal_node (  car open_list ) )

            ;reformat the answer.
            ( setf state_list ( reformat goal_node ) )

            ;( print node_count )
            ( return state_list )

        )

        ;when the open_list is empty don't do anything!!        
        ( when ( null open_list ) 
            ( return nil ) 
        )


        ; get current node from OPEN, update OPEN and CLOSED
        ( setf current     ( car  open_list           ) )
        ( setf open_list   ( cdr  open_list           ) )
        ( setf closed_list ( cons current closed_list ) )


        ;we're about to expand another node, so increment counter
        ( incf *expanded* )
        ( setf successor_lst ( successors (node-state current)  n_value ) )

        ; add successors of current node to OPEN

        ( setf *generated* ( + *generated* ( length successor_lst ) ) )
        ;( format t "CURRENT   : ~A~%" (node-state current) )
        ;( format t "SUCCESSORS: ~A~%~%" successor_lst )
        
        ;( setf node_count ( + ( length successor_lst ) node_count ) )
        ;( format t "Successors Generated: ~D~%" node_count )
        ; for each successor node
        ( loop for s in successor_lst do 

            ; if the node is not on OPEN or CLOSED
            ; add to the end of the list
            ( setf temp_node  ( make-node :state s :parent current ) )
            ;probably change this to COND so we can update the stats
            ( cond
                (
                    ;if both these conditions are not met
                    ( and
                      ( not ( member temp_node open_list   :test #'equal-states) )
                      ( not ( member temp_node closed_list :test #'equal-states) )
                    )
                    
                    ;add the successor to the open list
                    ;added in node struct to preserve path back to start
                    
                    ;increment number of distinct nodes
                    ( incf *distinct* )
                    ( setf open_list 

                        ;append first item then second item,
                        ;in this case the rest of the open list is the 
                        ;first item, the new successor is the second item.
                        ( append open_list  ( list temp_node ) )
                    )
                )
            )
        )
    )
)

#|--------------------------------------------------------------------------|#
#|                           Helper Functions                               |#
#|--------------------------------------------------------------------------|#
( defun reformat ( goal_node )
    ( do*
        (
            ( state_list ( list ( node-state goal_node ) ) )
            ( current ( node-parent goal_node) )
        )

        ;DO* exit condition:
        ;do this until we get back to the beginning, where our start state is
        ( ( null ( node-parent  current ) ) 

            ;youre at the beginning, but remember to add that one to the list too!
            ( setf state_list ( append  
                                ( list ( node-state current ) ) 
                                state_list ) 
            )

            ;This is our answer
            ( return state_list )
        )

        ;check if current is null for whatever reason
        ( when ( null current ) 
            ( return nil ) 
        )

        ;Set nodes into state list in reverse order
        ( setf state_list ( append  ( list ( node-state current ) ) state_list ) )

        ;the next node to inspect is the parent of the current node
        ( setf current ( node-parent current ) )
    )
)

; Member-state looks for a node on the node-list with the same state.
( defun member-state ( state node-list )
    ( dolist ( node node-list )
        ( when ( equal state ( node-state node ) ) 
            ( return node ) 
        )
    )
)
