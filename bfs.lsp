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
( load 'mapper )

#|--------------------------------------------------------------------------|#
#|                                Structs                                   |#
#|--------------------------------------------------------------------------|# 

; Initialize a Node Structure
( defstruct node state parent )

#|--------------------------------------------------------------------------|#
#|                              BFS Functions                               |#
#|--------------------------------------------------------------------------|#
( defun bfs ( puz_state )
    (do*
        (
            (current ( make-node :state puz_state :parent nil ) )
            ( open_list (list current) )
            ( closed_list   nil )
            ( successor_lst nil )
            ( return_list   nil )
            ( new_node      nil )
            ( g_state ( generate_goal ( - ( length puz_state ) 1) ) )

            ( node_count 0 )
        )

        ;DO* exit condition:
        ;Check the state of the node at the front of the open list.
        ;If this value is 
        ( ( goal? ( node-state ( car open_list ) ) g_state ) 

            ;This is our answer
            ( setf goal_node (  car open_list ) )

            ;reformat the answer.
            ( setf state_list ( reformat goal_node ) )


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


        ; add successors of current node to OPEN
        ( setf successor_lst ( successors (node-state current) ) )

        ;( setf node_count ( + ( length successor_lst ) node_count ) )
        ;( format t "Successors Generated: ~D~%" node_count )
        ; for each successor node
        ( loop for s in successor_lst do 

            ; if the node is not on OPEN or CLOSED
            ; add to the end of the list

            ;probably change this to COND so we can update the stats
            ( if 
                ;if both these conditions are not met
                ( and
                  ( not ( member s open_list   ) )
                  ( not ( member s closed_list ) )
                )
                
                ;add the successor to the open list
                ;added in node struct to preserve path back to start
                ( setf open_list 

                    ;append first item then second item,
                    ;in this case the rest of the open list is the 
                    ;first item, the new successor is the second item.
                    ( append open_list 
                            ( list 
                                ( make-node :state s :parent current )
                            )
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
        ( setf state_list ( append  (list (node-state current)) state_list ) )

        ;the next node to inspect is the parent of the current node
        ( setf current (node-parent current))

    )
)