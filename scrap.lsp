
#|--------------------------------------------------------------------------|#
#|                             search-funcs.lsp                             |#
#|--------------------------------------------------------------------------|#

 #|( defun count_wrong ( state goal )
    ( cond
        ( ( or ( not state ) ( not goal ) ) 0 )

        ( ( = ( car state ) ( car goal ) )
            ( count_wrong ( cdr state ) ( cdr goal ) )
        )

        ( t
            ( + ( count_wrong ( cdr state ) ( cdr goal ) ) 1 )
        )
    )
)|#

#|; Generates a goal state for an n-puzzle given the value of n.
( defun generate-goal ( puzzle-size )
    ( let
        (
            ( lst NIL )    ; puzzle expressed in spiral (clock-wise) order
        )

        ; Creates list whose values range from 1 to puzzle-size in order
        ( do ( ( i 1 ( 1+ i ) ) )
            ( ( > i puzzle-size ) )
            ( setf lst ( cons i lst ) )
        )
        ; Adds 0 to end of the list
        ( setf lst ( cons 0 lst ) )

        ; Changes lst from spiral order to row-major order
        ( spiral-to-rows ( reverse lst ) )
    )
)|#

;( load 'mapper )
#|
( defun compare_test ( n )

    ; n = 115599 is a good test that shows the difference in speed
    ( let ( lst1 lst2 lst3 cmp-str )
        ( format t "mapper: " )
        ( setf lst1 ( generate_goal n ) )
        ( format t "~A~%" lst1 )

        ( format t "spiral: " )
        ( setf lst2 ( generate-goal n ) )
        ( format t "~A~%" lst2 )
    
        ( format t "mapper: " )
        ( setf lst3 ( generate_goal n ) )
        ( format t "~A~%" lst3 )

        ( if ( equal lst1 lst2 )
            ( setf cmp-str "" )
            ( setf cmp-str "not " )
        )
        ( format t "The lists are ~Athe same.~%" cmp-str )
    )

)
|#

#|
( defun count_wrong_w_rot ( state goal )
    ( let
        (
            ( lst ( copy-list state ) ) ; Local copy of state
            ( count 0 ) ; The number of tiles out of place
            ( puz-size ( isqrt ( length state ) ) ) ; Side length of the puzzle
            correct-pos ; The correct position of a tile
        )
        
        ( cond

            ; Catch for if the state is an inappropriate length
            ( ( /= ( length lst ) ( length goal ) )
                NIL
            )
            
            ( t
                ; For i = 0 .. length of lst
                ( do
                    (
                        ( i 0 ( 1+ i ) )
                    )
                    ( ( >= i ( length lst ) ) count )

                    ; While the tile at position i is out of place
                    ( do ()
                        ( ( eq ( nth i lst ) ( nth i goal ) ) )
                        
                        ( setf correct-pos ( position ( nth i lst ) goal ) )
                        
                        ( setf count ( + count
                            ( abs ( - ( floor i puz-size ) ( floor correct-pos puz-size ) ) )
                        ) )
                    
                        ( setf count ( + count
                            ( abs ( - ( mod i puz-size ) ( mod correct-pos puz-size ) ) )
                        ) )
                       
                        ( rotatef ( nth i lst ) ( nth correct-pos lst ) )
                    )
                )
            )
        )
    )
)|#

#|
( let ( ( count -1 ) )
    ( defun bad-heuristic ( state )
        ( setf count ( 1+ count ) )
        count
    )
)
|#

#|--------------------------------------------------------------------------|#
#|                               a_star.lsp                                 |#
#|--------------------------------------------------------------------------|#

;( defun state_equal ( node1 node2 )
;    ( equal ( caddr node1 ) ( caddr node2 ) )
;)

;( defun reformat_node ( node )
;    ( list ( nth 3 node ) ( nth 2 node ) )
;)

;( defun make_node ( g_val state parent)
;    ( append ( list g_val ( heuristic state ) state ) ( cddr parent ) )
;)

;( defun make_node ( g_val state parent)
;    ( list g_val ( heuristic state ) state ( caddr parent ) )
;)

;( let ( ( inc 0 ) )
;    ( defun test ()
;        ( format t "~D~%" inc )
;        ( setf inc ( 1+ inc ) )
;    )
;)

;( defun test_list ()
;    '( ( 1 0 ( 1 2 3 4 5 6 7 8 0 ) ) ( 1 1 ( 0 8 7 6 5 4 3 2 1 ) ) )
;)

#|
; Iterative version of the a*, only returns if a solution was found.
( defun A*_search ( open_list closed_list goal? successors heuristic )
    ( let
        (
            ; Gets the best node in the Open List
            ( best ( find_best open_list ) )
            ; Holds '((open_list) (closed_list)) which
            ; is returned by some functions
            both
            succ_list    ; List of successor nodes
            return_list    ; List that is returned by this function
        )
        
        ( do ()
            ( ( funcall goal? ( caddr best ) ) NIL )
            
            ; Moves best to Closed List
            ( setf both
                ( mov_elem_between_lsts best open_list closed_list )
            )
            ( setf open_list ( car both ) )
            ( setf closed_list ( cadr both ) )

            ; Generates list of successors
            ( setf succ_list
                ( map
                    'list
                    #'( lambda ( state )
                        ( make_node state best heuristic )
                    )
                    ( funcall successors ( caddr best ) )
                )
            )
            
            ; Update number of nodes expanded
            ( setf *expanded* ( + *expanded* 1 ) )
            
            ; Update number of nodes generated
            ( setf *generated* ( + *generated* ( length succ_list ) ) )

            ; Processes successors
            ( setf both ( process_succs succ_list open_list closed_list ) )
            ( setf open_list ( car both ) )
            ( setf closed_list ( cadr both ) )
            
            ( setf best ( find_best open_list ) )
        )
        
        ( format t "Solution Found" )
        NIL
    )
)
|#

#|
( defun process_succs ( succ_list open_list closed_list )
    ( let ( extra )
        ( dolist ( succ succ_list ( list open_list closed_list ) )
            ( cond

                ; If the same state was found on the Closed List:
                ( ( setf extra
                        ( get_node_with_state ( caddr succ ) closed_list )
                    )
                    ; If succ is better than extra:
                    ( when ( < ( eval_node succ ) ( eval_node extra ) )
                        ; Removes extra and puts succ on Open List
                        ( setf closed_list ( remove extra closed_list ) )
                        ( setf open_list ( cons succ open_list ) )
                    )
                )

                ; If the same state was found on the Open List:
                ( ( setf extra
                        ( get_node_with_state ( caddr succ ) open_list )
                    )
                    ; If succ is better than extra:
                    ( when ( < ( eval_node succ ) ( eval_node extra ) )
                        ; Removes extra and puts succ on Open List
                        ( setf open_list ( remove extra open_list ) )
                        ( setf open_list ( cons succ open_list ) )
                    )
                )

                ; If no extras were found:
                ( t
                    ; Puts succ on Open List
                    ( setf open_list ( cons succ open_list ) )
                    
                    ; Update number of distinct nodes
                    ( setf *distinct* ( + *distinct* 1 ) )
                )
                
            )
        )
        
        ; Return updated lists
        ;( list open_list closed_list )
    )
)
|#

#|
( defun find_best ( open_list )
    ;( format t "Loop Call" )
    ( when ( listp open_list )
        ( let ( ( best ( car open_list ) ) )
            ( dolist ( i ( cdr open_list ) best )
                ; If a better node is found:
                ( when ( < ( eval_node i ) ( eval_node best ) )
                    ; Update best node
                    ( setf best i )
                )
            )
        )
    )
)
|#

;Because the heuristic function
;will usually differ from problem to problem, it has been given its own section
;on this file to make it easy to find and change.

;These functions require that two functions be specified. The first function
;required is of the form ( goal? state ) which takes a state and returns T if
;the state given is the goal state or NIL otherwise. The second function
;required is of the form ( successors state ) which takes a state and returns
;a list of successor states. This function will dictate how the algorithm will
;traverse the state-space. These functions may be specified in a file that is
;loaded in under the Files Loaded section.

(defun user-defined-variables (&optional (package :cl-user))
  (loop with package = (find-package package)
        for symbol being the symbols of package
        when (and (eq (symbol-package symbol) package)
                  (boundp symbol))
          collect symbol))
