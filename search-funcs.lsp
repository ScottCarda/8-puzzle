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

; Generates the successors of an n-puzzle at the given state.
( defun successors ( state )
    "Generates the successor of an n-puzzle at the given state."
    ( let (
            ;if 8-puzzle, then n = 8
            ;sqrt(8+1) = 3, so 3x3 puzzle
            ;if 15-puzzle, then n = 15
            ;sqrt(15+1) = 4, so 4x4 puzzle, etc...            
            ( dimension ( sqrt ( length state ) ) )

            ( location ( position 0 state ) ) ; Where the 0 is in the puzzle
            ( succ '() ) ; List of successors to be returned
            UP DOWN LEFT RIGHT ; Possible successors
          )
        
        ; Perform UP move and add it to successors

        ;Can only go up if location is greater than (dimension value - 1)
        ;ex: 8 puzzle is 3x3, so can only move up if location is > 2 (aka 3-1)
        ( when ( > location ( - dimension 1 ) )
            ( setf UP ( copy-list state ) )

            ; Move tile over a value equal to the dimension
            ( rotatef ( nth location UP ) ( nth ( - location dimension ) UP ) )
            ( setf succ ( cons UP succ ) )
        )
        
        ; Perform DOWN move and add it to successors

        ;Can only go up if location is less than ( n value + 1 - dimension value)
        ;ex: 8 puzzle  is 3x3, so can only move down if location is <  6 (aka 8+1-3)
        ;ex: 15 puzzle is 4x4, so can only move down if location is < 12 (aka 15+1-4)
        ( when ( < location ( - ( length state ) dimension ) )
            ( setf DOWN ( copy-list state ) )

            ; Move tile over a value equal to the dimension
            ( rotatef ( nth location DOWN ) ( nth ( + location dimension ) DOWN ) )
            ( setf succ ( cons DOWN succ ) )
        )
        
        ; Perform LEFT move and add it to successors
        ( when ( > ( mod location dimension ) 0 )
            ( setf LEFT ( copy-list state ) )
            
            ; Move tile to the left by one
            ( rotatef ( nth location LEFT ) ( nth ( - location 1 ) LEFT ) )
            ( setf succ ( cons LEFT succ ) )
        )
        
        ; Perform RIGHT move and add it to successors
        ( when ( < ( mod location dimension ) ( - dimension 1 ) )
            ( setf RIGHT ( copy-list state ) )
            
            ; Move tile to the right by one
            ( rotatef ( nth location RIGHT ) ( nth ( + location 1 ) RIGHT ) )
            ( setf succ ( cons RIGHT succ ) )
        )
    succ
    )
)



#|--------------------------------------------------------------------------|#
#|                          Determine Goal State                            |#
#|--------------------------------------------------------------------------|#

; Determines if you have reached a goal state, perhaps a bit of overkill.
( defun goal? (
    curr_state 
    &optional (goal_state '(1 2 3 8 0 4 7 6 5))
    )
    "Predicate function that determines if given state is the goal state."
    ( let ( check_list found_goal )
        ; Check each spot in the list with the goal
        (setf check_list (mapcar #'eq curr_state goal_state))

        ; Found_goal will be NIL if everything matches
        (setf found_goal (position NIL check_list))

        ; Returns T if goal reached, NIL otherwise
        (null found_goal)
    )
)

; Generates a goal state for an n-puzzle given the value of n.
( defun generate-goal ( puzzle-size )
    "Generates a goal state for an n-puzzle given the value of n."
    ( let
        (
            ( lst NIL )    ; Puzzle expressed in spiral (clock-wise) order
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
    "Converts puzzle from row-major order to spiral order."
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
    "Converts puzzle from spiral order to row-major order."
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
)

#|--------------------------------------------------------------------------|#
#|                            Heuristic Function                            |#
#|--------------------------------------------------------------------------|#

; Heuristic for how close a state is to the
; goal state based on number of tiles wrong.
( defun count_wrong ( state goal )
    "Counts the number of tiles out of place and divides by two."
    ( let
        (
            ( count 0 ) ; The number of tiles out of place
        )
        
        ( cond
            
            ; Catch for if the state is an inappropriate length
            ( ( /= ( length state ) ( length goal ) )
                NIL
            )
            
            ( t
                ; For i = 0 .. lenght of state
                ( do
                    (
                        ( i 0 ( 1+ i ) )
                    )
                    ; One move will change the place of two tiles, so divide the count by two
                    ( ( >= i ( length state ) ) ( / count 2 ) )
                    
                    ; If a tile is out of place, increment count
                    ( when ( not ( eq ( nth i state ) ( nth i goal ) ) )
                        ( setf count ( + 1 count ) )
                    )
                )
            )
        )
    )
)

; Heuristic for how close a state is to the goal state based
; on the Manhattan distance of the tiles out of place.
( defun count_wrong_w_dist ( state goal )
    "Counts the Manhattan distance of the tiles out of place and divides by two."
    ( let
        (
            ( count 0 ) ; The number of tiles out of place
            ( puz-size ( isqrt ( length state ) ) ) ; Side length of the puzzle
            correct-pos ; The correct position of a tile
        )
        
        ( cond

            ; Catch for if the state is an inappropriate length
            ( ( /= ( length state ) ( length goal ) )
                NIL
            )

            ( t
                ; For i = 0 .. length of state
                ( do
                    (
                        ( i 0 ( 1+ i ) )
                    )
                    ; One move will change the place of two tiles, so divide the count by two
                    ( ( >= i ( length state ) ) ( / count 2 ) )

                    ; If a tile is out of place:
                    ( when ( not ( eq ( nth i state ) ( nth i goal ) ) )

                        ; Find the correct position
                        ( setf correct-pos ( position ( nth i state ) goal ) )

                        ; Increment count by the number of rows off
                        ( setf count ( + count
                            ( abs ( - ( floor i puz-size ) ( floor correct-pos puz-size ) ) )
                        ) )

                        ; Increment count by the number of columns off
                        ( setf count ( + count
                            ( abs ( - ( mod i puz-size ) ( mod correct-pos puz-size ) ) )
                        ) )
                    )
                )
            )
        )
    )
)

; Heuristic for how close a state is to the goal state based
; on the Manhattan distance of the tiles out of place. Corrects
; tiles out of place as it finds them.
( defun count_wrong_w_rot ( state goal )
    "Counts the Manhattan distance of the tiles
    out of place, moveing error tiles as it finds them."
    ( let
        (
            ( lst ( copy-list state ) ) ; Local copy of state
            ( count 0 ) ; The number of tiles out of place
            ( puz-size ( isqrt ( length state ) ) ) ; Side length of the puzzle
            fetch-pos ; The position of a tile to be fetched
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

                    ; If a tile is out of place:
                    ( when ( not ( eq ( nth i state ) ( nth i goal ) ) )
                        
                        ; Find the position of the tile that should be at this position
                        ( setf fetch-pos ( position ( nth i goal ) lst ) )
                        
                        ; Increment count by the number of rows off
                        ( setf count ( + count
                            ( abs ( - ( floor i puz-size ) ( floor fetch-pos puz-size ) ) )
                        ) )

                        ; Increment count by the number of columns off
                        ( setf count ( + count
                            ( abs ( - ( mod i puz-size ) ( mod fetch-pos puz-size ) ) )
                        ) )

                        ; Swaps the tiles
                        ( rotatef ( nth i lst ) ( nth fetch-pos lst ) )
                    )
                )
            )
        )
    )
)

( defun count_wrong_w_nilsson_score ( state goal ) 
    ( let
        (
            ( lst ( copy-list state ) ) ; Local copy of state
            ( count 0 ) ; The number of tiles out of place
            ( puz-size ( isqrt ( length state ) ) ) ; Side length of the puzzle
            fetch-pos ; The position of a tile to be fetched
	    ( nilsson 0 );nilsson sequence score
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

                    ; If a tile is out of place:
                    ( when ( not ( eq ( nth i state ) ( nth i goal ) ) )
                        
                        ; Find the position of the tile that should be at this position
                        ( setf fetch-pos ( position ( nth i goal ) lst ) )
                        
                        ; Increment count by the number of rows off
                        ( setf count ( + count
                            ( abs ( - ( floor i puz-size ) ( floor fetch-pos puz-size ) ) )
                        ) )

                        ; Increment count by the number of columns off
                        ( setf count ( + count
                            ( abs ( - ( mod i puz-size ) ( mod fetch-pos puz-size ) ) )
                        ) )

                        ; Swaps the tiles
                        ( rotatef ( nth i lst ) ( nth fetch-pos lst ) )
                    )
                )
		; For i = 0 .. length of lst
                ( do
                    (
                        ( i 0 ( 1+ i ) )
                    )
                    
                    ( ( >= i ( length state ) ) count )
                    ;Checks each tile to see if it is out of place
                    ( when ( not ( eq ( nth i state ) ( nth i goal ) ) )
			;Adds 2 to nilsson score for non center, 1 if center is off
			(if ( eq ( nth i goal ) 0 )
			    ( setf nilsson ( + 1 nilsson ) )
                            ( setf nilsson ( + 2 nilsson ) )
			)
                    )
                )
		( + count nilsson )
            )
        )
    )
)
#|
 | admis: number of values out of place with consideration for distance needed to travel ( div 2 )
 | admis: number of values out of place ( minus one )
 | inadmis: comparing sums of rows and columns
 |#
 
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
