#|
                    ***** SEARCH-FUNCS.LSP *****

Contains routines that are specific to n-puzzle state-space searches.
This includes a function for generating successors to a state, functions for
defining and checking for goal states, a function for checking if a state is
solvable, and functions for converting between puzzle formats.

Authors: J. Anthony Brackins, Scott Carda, Leif Torgersen
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#

#|--------------------------------------------------------------------------|#
#|                           Successors Function                            |#
#|--------------------------------------------------------------------------|#

; Generates the successors of an n-puzzle at the given state.
( defun successors ( state )
    "Generates the successor of an n-puzzle at the given state."
    ( let (
        ; If 8-puzzle, then n = 8
        ; sqrt(8+1) = 3, so 3x3 puzzle
        ; if 15-puzzle, then n = 15
        ; sqrt(15+1) = 4, so 4x4 puzzle, etc...            
        ( dimension ( sqrt ( length state ) ) )

        ( location ( position 0 state ) ) ; Where the 0 is in the puzzle
        ( succ '() ) ; List of successors to be returned
        UP DOWN LEFT RIGHT ; Possible successors
      )
        
        ; Perform UP move and add it to successors

        ; Can only go up if location is greater than (dimension value - 1)
        ; ex: 8 puzzle is 3x3, so can only move up if location is > 2 (aka 3-1)
        ( when ( > location ( - dimension 1 ) )
            ( setf UP ( copy-list state ) )

            ; Move tile over a value equal to the dimension
            ( rotatef ( nth location UP ) ( nth ( - location dimension ) UP ) )
            ( setf succ ( cons UP succ ) )
        )
        
        ; Perform DOWN move and add it to successors

        ; Can only go up if location is less than
        ; ( n value + 1 - dimension value)
        ; Ex: 8 puzzle  is 3x3, so can only move down if
        ; location is <  6 (aka 8+1-3)
        ; Ex: 15 puzzle is 4x4, so can only move down if
        ; location is < 12 (aka 15+1-4)
        ( when ( < location ( - ( length state ) dimension ) )
            ( setf DOWN ( copy-list state ) )

            ; Move tile over a value equal to the dimension
            ( rotatef
                ( nth location DOWN )
                ( nth ( + location dimension ) DOWN )
            )
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
#|                           Goal State Functions                           |#
#|--------------------------------------------------------------------------|#

; Determines if you have reached a goal state, perhaps a bit of overkill.
( defun goal? (
    curr_state 
    &optional ( goal_state '( 1 2 3 8 0 4 7 6 5 ) )
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
            ( lst NIL ) ; Puzzle expressed in spiral (clock-wise) order
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

#|--------------------------------------------------------------------------|#
#|                    Puzzle Format Conversion Functions                    |#
#|--------------------------------------------------------------------------|#

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
                    ( setf spiral
                        ( nconc spiral ( list ( nth i temp-lst ) ) )
                    )
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
            ( i 1 ( 1+ i ) ) ; Side length of puzzle built in each iteration

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
#|                            Solvable Predicate                            |#
#|--------------------------------------------------------------------------|#

; Determines if the puzzle is solvable ( able to be ordered in a clock-wise
; spiral ) by counting the number of inversions in the puzzle
( defun solvablep ( puzzle )
    "Predicate function for determining if the given puzzle is solvable."
    ( let
        (
            ( count 0 ) ; Count of inversions
            ( dimension ( isqrt ( length puzzle ) ) ) ; Side length of puzzle
            row-with-blank ; Row, starting at zero, of that has the blank
        )
        
        ; Calculate row-with-blank using dimension
        ( setf row-with-blank ( floor ( position 0 puzzle ) dimension ) )
        
        ; For each element of lst, count the number
        ; of preceding elements whose value is less 
        ( do
            (
                ( i 0 ( 1+ i ) ) ; Loop variable
            )
            ( ( >= i ( length puzzle ) ) count )
            
            ; Skip zero
            ( unless ( = ( nth i puzzle ) 0 )
                ; Loop through all elements of list after i
                ( do
                    (
                        ( j i ( 1+ j ) ) ; Loop variable
                    )
                    ( ( >= j ( length puzzle ) ) ) ; Stop at end of list

                    ; Count one for each element j whose
                    ; value is less than that of element i
                    ( when
                        ( and
                            ( > ( nth i puzzle ) ( nth j puzzle ) )
                            ( /= ( nth j puzzle ) 0 ) ; Skip zero
                        )
                        ( setf count ( 1+ count ) )
                    )
                )
            )
        )
        
        ; Determine whether the state is solvable based on number of
        ; inversions, dimension, and which row the blank is on
        ( cond

            ; If dimension is odd and there are an even number of inversions:
            (
                ( and
                    ( oddp dimension )
                    ( oddp count )
                )
                ; Then the puzzle is solvable
                T
            )
            
            ; If dimension is even:
            (
                ( and
                    ( evenp dimension )
                    ; And the row with the blank is an
                    ; odd row with even inversions
                    ; or an even row with odd inversions
                    ( eq
                        ( oddp row-with-blank )
                        ( evenp count )
                    )
                )
                ; Then the puzzle is solvable
                T
            )

            ; Else, not solvable
            ( t NIL )
        )
    )
)





























