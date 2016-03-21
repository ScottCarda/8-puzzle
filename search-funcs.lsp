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
      &optional ( n_value 8 )        
    )
    ( let (
            ;if 8-puzzle, then n = 8
            ;sqrt(8+1) = 3, so 3x3 puzzle
            ;if 15-puzzle, then n = 15
            ;sqrt(15+1) = 4, so 4x4 puzzle, etc...            
            ( dimension ( sqrt ( + n_value 1 ) ) )

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
        ( when ( < location ( - ( + n_value 1 ) dimension ) )
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
              (puzzle_size 9))
    ( let ( check_list found_goal )
        ;Check each spot in the list with the goal
        (setf check_list (mapcar #'eq curr_state goal_state))

        ;found_goal will be NIL if everything matches
        (setf found_goal (position NIL check_list))

        ;goal? returns T if goal reached, NIL otherwise
        (null found_goal)
    )
)

; Takes a puzzle expressed in row-major order and returns the same puzzle
; expressed in spiral (clock-wise) order
( defun rows-to-spiral ( lst N )
    ( let
        (
            ( spiral NIL )  ; The puzzle listed in spiral format
            ( temp-lst ( copy-list lst ) )  ; Local copy of lst
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
                    ( setf spiral ( append spiral ( list ( nth i temp-lst ) ) ) )
                    ; Sets up top row for removal
                    ( setf ( nth i temp-lst ) -1 )
                )

                ; Gets right-most column
                ( do ( ( i ( - N 1 ) ( setf i ( + N i ) ) ) )
                    ( ( > i ( - ( * N N ) 1 ) ) )
                    ; Puts right-most column into spiral
                    ( setf spiral ( append spiral ( list ( nth i lst ) ) ) )
                    ; Sets up right-most column for removal
                    ( setf ( nth i temp-lst ) -1 )
                )

                ; Sets up N-1 by N-1 puzzle by removing top row and
                ; right-most column from the puzzle and reversing the puzzle
                ( setf temp-lst ( reverse ( remove -1 temp-lst ) ) )

                ; Recurses with N-1 by N-1 puzzle
                ( append spiral ( rows-to-spiral temp-lst ( - N 1 ) ) )
            )
        )        
    )
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
