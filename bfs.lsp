#|
                    ***** BFS.LSP *****

Routine for performing Breadth-First Search on the 8-puzzle. 
BFS is an exhaustive graph search routine that maintains 
OPEN and CLOSED lists in order to avoid cyclical conditions. 
Due to the exhaustive nature, heuristics are not implemented. 

BFS expands nodes one level at a time, as opposed to DFS which 
will explore the full deep path of a given node. In order to 
facilitate this in the program, newly generated nodes are added 
to the end of the OPEN list as successors are generated.

This program is based on the search.lsp code provided by 
Dr. Weiss on the MCS website, modified to work with our program.

Author:  John M. Weiss, Ph.D. , J. Anthony Brackins


Written Spring 2016 for CSC447/547 AI class.

Modifications: 
Added variables to monitor expanded, distinct, and generated nodes.
Wrote reformat method to convert node structure into a list of states
Generating successors and determining goal state are definied in 
search-funcs.lsp
|#


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

#|--------------------------------------------------------------------------|#
#|                              BFS Functions                               |#
#|--------------------------------------------------------------------------|#

; Test if two nodes have the same state.
( defun equal-states (n1 n2) 
    "Determine if two nodes contain identical states"
    ( equal (node-state n1) (node-state n2) )
)

( defun bfs ( puz_state 
              &optional ( goal_state nil ) 
            )
    "Perform the Breadth-First search algorithm on the n-puzzle"
    ;clear out global vars
    ( setf *generated* 0 )
    ( setf *distinct*  1 )
    ( setf *expanded*  0 )

    ( if ( null goal_state ) 
        ;TRUE: if goal_state is null, generate it
        ( bfs-search-do 
            puz_state 
            ( generate-goal 
                ( - 
                    ( length puz_state ) 
                    1
                ) 
            ) 
        )

        ;FALSE: just use the goal_state passed in
        ( bfs-search-do puz_state goal_state )
    )
)

( defun bfs-search-do ( puz_state g_state )
    "An iterative BFS approach using the do* method"
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
        ( setf successor_lst ( successors (node-state current) ) )

        ; add successors of current node to OPEN

        ( setf *generated* ( + *generated* ( length successor_lst ) ) )
        
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
                        ( not   ( member 
                            temp_node 
                            open_list   
                            :test #'equal-states
                        ) )
                        ( not   ( member 
                            temp_node 
                            closed_list 
                            :test #'equal-states
                        ) )
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
    "Collapse the node structure into list that can be printed to the terminal"
    ( do*
        (
            ( state_list ( list ( node-state goal_node ) ) )
            ( current ( node-parent goal_node) )
        )

        ;DO* exit condition:
        ;do this until we get back to the beginning, where our start state is
        ;( ( null ( node-parent  current ) ) 
        ( (or ( null current ) ( null (node-parent current ) ) )

            ;youre at the beginning, but 
            ;remember to add that one to the list too!
            ( if ( null current ) 

                ;TRUE: Don't do anything, this nil check is only to handle
                ;the rare case that the initial puzzle passed in was the 
                ;goal state, as this causes the node structure to not be 
                ;built properly.
                nil             

                ;FALSE: Go one step back and add the beginning of the list 
                ;to the list of states
                ( setf state_list 
                    ( append  
                        ( list ( node-state current ) ) 
                        state_list 
                    ) 
                ) 
            )

            ;This is our answer
            ( return state_list )
        )

        ;check if current is null for whatever reason
        ( when ( null current ) 
            ( return nil ) 
        )

        ;Set nodes into state list in reverse order
        ( setf state_list 
            ( append 
                ( list 
                    ( node-state current ) 
                ) 
                state_list 
            ) 
        )

        ;the next node to inspect is the parent of the current node
        ( setf current ( node-parent current ) )
    )
)

; Member-state looks for a node on the node-list with the same state.
( defun member-state ( state node-list )
    "Member-state looks for a node on the node-list with the same state."
    ( dolist ( node node-list )
        ( when ( equal state ( node-state node ) ) 
            ( return node ) 
        )
    )
)
