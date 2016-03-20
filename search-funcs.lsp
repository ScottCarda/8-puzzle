( defun successors ( state )
    ( let (
            ( location ( position 0 state ) )
            ( succ '() )
            UP DOWN LEFT RIGHT
          )
        
        ; Perform UP move and add it to successors
        ( when ( > location 2 )
            ( setf UP ( copy-list state ) )
            ( rotatef ( nth location UP ) ( nth ( - location 3 ) UP ) )
            ( setf succ ( cons UP succ ) )
        )
        
        ; Perform DOWN move and add it to successors
        ( when ( < location 6 )
            ( setf DOWN ( copy-list state ) )
            ( rotatef ( nth location DOWN ) ( nth ( + location 3 ) DOWN ) )
            ( setf succ ( cons DOWN succ ) )
        )
        
        ; Perform LEFT move and add it to successors
        ( when ( > ( mod location 3 ) 0 )
            ( setf LEFT ( copy-list state ) )
            ( rotatef ( nth location LEFT ) ( nth ( - location 1 ) LEFT ) )
            ( setf succ ( cons LEFT succ ) )
        )
        
        ; Perform RIGHT move and add it to successors
        ( when ( < ( mod location 3 ) 2 )
            ( setf RIGHT ( copy-list state ) )
            ( rotatef ( nth location RIGHT ) ( nth ( + location 1 ) RIGHT ) )
            ( setf succ ( cons RIGHT succ ) )
        )
	succ
    )
)


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
    ( - ( count_wrong state '( 1 2 3 8 0 4 7 6 5 ) ) 1 )
)

( defun count_wrong ( state goal )
	( cond
		( ( or ( not state ) ( not goal ) ) 0 )

		( ( = ( car state ) ( car goal ) )
			( count_wrong ( cdr state ) ( cdr goal ) )
		)

		( t
			( + ( count_wrong ( cdr state ) ( cdr goal ) ) 1 )
		)
	)
)

( defun count_wrong_w_dist ( state goal N )
    ( let
        (
            ( count 0 )
            correct-pos
        )
        ( do ( i 0 ( 1+ i ) )
            ( ( >= i ( length state ) ) )
        
            ( when ( not ( eq ( nth i state ) ( nth i goal ) ) )
                ( setf correct-pos ( position ( nth i state ) goal ) )
                
                ( setf count ( + count
                    ( abs ( - ( floor i N ) ( floor correct-pos N ) ) )
                ) )
                
                ( setf count ( + count
                    ( abs ( - ( mod i N ) ( mod correct-pos N ) ) )
                ) )
            )
        )
    )
)

#|
 | admis: number of values out of place ( minus one )
 | inadmis: number of values out ot place with consideration for distance needed to travel
 | inadmis: comparing sums of rows and columns
 |#
