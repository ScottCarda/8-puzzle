#|
                    ***** SEARCH-FUNCS.LSP *****

Routines for generating successor states and determining 
Goal states have been reached.

Authors: J. Anthony Brackins, Scott Carda, Leif Torgersen
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#


#|--------------------------------------------------------------------------|#
#|                           SUCCESSORS FUNCTION                            |#
#|--------------------------------------------------------------------------|#
;new successor function designed to generalize to the n-puzzle
( defun successors 
    ( 
      state 
    )
    ( let (
            ;if 8-puzzle, then n = 8
            ;sqrt(8+1) = 3, so 3x3 puzzle
            ;if 15-puzzle, then n = 15
            ;sqrt(15+1) = 4, so 4x4 puzzle, etc...            
            ( dimension ( sqrt ( length state ) ) )

            ( location ( position 0 state ) )
            ( succ '() )
            UP DOWN LEFT RIGHT
          )
        
        ; Perform UP move and add it to successors

        ;Can only go up if location is greater than (dimension value - 1)
        ;ex: 8 puzzle is 3x3, so can only move up if location is > 2 (aka 3-1)
        ( when ( > location (- dimension 1) )
            ( setf UP ( copy-list state ) )

            ;move tile over a value equal to the dimension
            ( rotatef ( nth location UP ) ( nth ( - location dimension ) UP ) )
            ( setf succ ( cons UP succ ) )
        )
        
        ; Perform DOWN move and add it to successors

        ;Can only go up if location is less than ( n value + 1 - dimension value)
        ;ex: 8 puzzle  is 3x3, so can only move down if location is <  6 (aka 8+1-3)
        ;ex: 15 puzzle is 4x4, so can only move down if location is < 12 (aka 15+1-4)
        ( when ( < location ( - ( length state ) dimension ) )
            ( setf DOWN ( copy-list state ) )

            ;move tile over a value equal to the dimension
            ( rotatef ( nth location DOWN ) ( nth ( + location dimension ) DOWN ) )
            ( setf succ ( cons DOWN succ ) )
        )
        
        ; Perform LEFT move and add it to successors
        ( when ( > ( mod location dimension ) 0 )
            ( setf LEFT ( copy-list state ) )
            ( rotatef ( nth location LEFT ) ( nth ( - location 1 ) LEFT ) )
            ( setf succ ( cons LEFT succ ) )
        )
        
        ; Perform RIGHT move and add it to successors
        ( when ( < ( mod location dimension ) 2 )
            ( setf RIGHT ( copy-list state ) )
            ( rotatef ( nth location RIGHT ) ( nth ( + location 1 ) RIGHT ) )
            ( setf succ ( cons RIGHT succ ) )
        )
    succ
    )
)



#|--------------------------------------------------------------------------|#
#|                          Determine Goal State                            |#
#|--------------------------------------------------------------------------|#
;Determine if you have reached a goal state, perhaps a bit of overkill
( defun goal? (
    curr_state 
    &optional (goal_state '(1 2 3 8 0 4 7 6 5))
    )
    ( let ( check_list found_goal )
        ;Check each spot in the list with the goal
        (setf check_list (mapcar #'eq curr_state goal_state))

        ;found_goal will be NIL if everything matches
        (setf found_goal (position NIL check_list))

        ;goal? returns T if goal reached, NIL otherwise
        (null found_goal)
    )
)

; Generates a goal state for an n-puzzle given the value of n.
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
)

; Takes a puzzle expressed in row-major order and returns the same puzzle
; expressed in spiral (clock-wise) order.
( defun rows-to-spiral ( lst )
    ( let
        (
            ( temp-lst ( copy-list lst ) )  ; Local copy of lst
            ( N ( isqrt ( length lst ) ) ) ; Length of a side of the puzzle
            ( spiral NIL )  ; The puzzle listed in spiral format
        )
        
        ( cond
            ; Base case of list with only one element
            ( ( = ( length lst ) 1 ) lst )

            ; Else gets top row and right-most column and recurse
            ( t
                ; Gets top row
                ( do ( ( i 0 ( 1+ i ) ) )
                    ( ( >= i ( - N 1 ) ) )
                    ; Puts top row into spiral
                    ( setf spiral ( nconc spiral ( list ( nth i temp-lst ) ) ) )
                    ; Sets up top row for removal
                    ( setf ( nth i temp-lst ) -1 )
                )

                ; Gets right-most column
                ( do ( ( i ( - N 1 ) ( setf i ( + N i ) ) ) )
                    ( ( > i ( - ( * N N ) 1 ) ) )
                    ; Puts right-most column into spiral
                    ( setf spiral ( nconc spiral ( list ( nth i lst ) ) ) )
                    ; Sets up right-most column for removal
                    ( setf ( nth i temp-lst ) -1 )
                )

                ; Sets up N-1 by N-1 puzzle by removing top row and
                ; right-most column from the puzzle and reversing the puzzle
                ( setf temp-lst ( reverse ( remove -1 temp-lst ) ) )

                ; Recurses with N-1 by N-1 puzzle
                ( nconc spiral ( rows-to-spiral temp-lst ) )
            )
        )        
    )
)

; Takes a puzzle expressed in spiral (clock-wise) order and
; returns the same puzzle expressed in row-major order.
( defun spiral-to-rows ( lst )
    ;( let
    ;    (
    ;        ( temp-lst ( copy-list lst ) ) ; Local copy of lst
    ;        ( rows NIL ) ; The puzzle listed in row-major order
    ;        ( transfer NIL ) ; An intermediary list
    ;    )

        ( do
            (
                ( i 1 ( 1+ i ) ) ; Side length of the puzzle built in each iteration

                ( temp-lst ( copy-list lst ) ) ; Local copy of lst
                ( rows NIL ) ; The puzzle listed in row-major order
                ( transfer NIL ) ; An intermediary list
                ( odd ) ; The ith odd number
            )
            ( ( not temp-lst ) rows ) ; Stops when whole list is processed

            ; Calculates odd
            ( setf odd ( - ( * 2 i ) 1 ) )

            ; Grabs the last ith odd number of elements from temp-lst
            ( setf transfer ( last temp-lst odd ) )
            ( setf temp-lst ( nbutlast temp-lst odd ) )

            ; Reverses the puzzle, rather than special-casing
            ; the bottom row and left-most column
            ( setf rows ( reverse rows ) )
            
            ; The first i elements from transfer are the top row
            ( setf rows ( nconc ( subseq transfer 0 i ) rows ) )
            ( setf transfer ( subseq transfer i ) )

            ; Rest of transfer is right-most column
            ( do ( ( j odd ( + i j ) ) )
                ( ( not transfer ) )
                ; A reported bug in Lisp prevents us from pushing onto an nthCDR
                ; so we use CDR of nthCDR and subtract 1 from j
                ( push ( pop transfer ) ( cdr ( nthcdr ( - j 1 ) rows ) ) )
            )

        )    

    ;)
)

#|--------------------------------------------------------------------------|#
#|                            Heuristic Function                            |#
#|--------------------------------------------------------------------------|#

; Heuristic function used to estimate the
; distance a state is from the goal state.
( defun heuristic ( state )
    ;( count_wrong state '( 1 2 3 8 0 4 7 6 5 ) )
    0
)

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

( defun count_wrong ( state goal )
    ( let
        (
            ( count 0 )
        )
        
        ( cond
            
            ( ( /= ( length state ) ( length goal ) )
                NIL
            )
            
            ( t
                ( do
                    (
                        ( i 0 ( 1+ i ) )
                    )
                    
                    ( ( >= i ( length state ) ) count )
                    
                    ( when ( not ( eq ( nth i state ) ( nth i goal ) ) )
                        ( setf count ( + 1 count ) )
                    )
                )
            )
        )
    )
)

( defun count_wrong_w_dist ( state goal )
    ( let
        (
            ( count 0 )
            ( puz-size ( isqrt ( length state ) ) )
            correct-pos
        )
        
        ( cond
    
            ( ( /= ( length state ) ( length goal ) )
                NIL
            )
            
            ( t    
                ( do
                    (
                        ( i 0 ( 1+ i ) )
                    )
            
                    ( ( >= i ( length state ) ) ( / count 2 ) )
            
                    ( when ( not ( eq ( nth i state ) ( nth i goal ) ) )
                        ( setf correct-pos ( position ( nth i state ) goal ) )
                    
                        ( setf count ( + count
                            ( abs ( - ( floor i puz-size ) ( floor correct-pos puz-size ) ) )
                        ) )
                    
                        ( setf count ( + count
                            ( abs ( - ( mod i puz-size ) ( mod correct-pos puz-size ) ) )
                        ) )
                    )
                )
            )
        )
    )
)

( defun count_wrong_w_rot ( state goal )
    ( let
        (
            ( lst ( copy-list state ) )
            ( count 0 )
            ( puz-size ( isqrt ( length state ) ) )
            correct-pos
        )
        
        ( cond
    
            ( ( /= ( length lst ) ( length goal ) )
                NIL
            )
            
            ( t    
                ( do
                    (
                        ( i 0 ( 1+ i ) )
                    )
            
                    ( ( >= i ( length lst ) ) count )
            
                    ( do ()
                        ( ( eq ( nth i lst ) ( nth i goal ) ) )
                        
                        ( setf correct-pos ( position ( nth i lst ) goal ) )
                    
                        ( setf count ( + count
                            ( abs ( - ( floor i puz-size ) ( floor correct-pos puz-size ) ) )
                        ) )
                    
                        ( setf count ( + count
                            ( abs ( - ( mod i puz-size ) ( mod correct-pos puz-size ) ) )
                        ) )
                        
                        ;( format t "Position ~D: ~A was changed to " i lst )
                        ( rotatef ( nth i lst ) ( nth correct-pos lst ) )
                        ;( format t "~A" lst )
                        
                    )
                )
            )
        )
    )
)

#|
 | admis: number of values out of place with consideration for distance needed to travel ( div 2 )
 | admis: number of values out of place ( minus one )
 | inadmis: comparing sums of rows and columns
 |#

( load 'mapper )

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

( defun compair_test ( n )

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
