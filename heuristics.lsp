#|
                    ***** HEURISTICS.LSP *****

Contains the Heuristics for A* search algorithms. Contains both 
Admissible and Inadmissible heuristics.

Authors: J. Anthony Brackins, Scott Carda, Leif Torgersen
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#

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
                ; For i = 0 .. length of state
                ( do
                    (
                        ( i 0 ( 1+ i ) )
                    )
                    ; One move will change the place of
                    ; two tiles, so divide the count by two
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
    "Counts the Manhattan distance of the
    tiles out of place and divides by two."
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
                    ; One move will change the place of two
                    ; tiles, so divide the count by two
                    ( ( >= i ( length state ) ) ( / count 2 ) )

                    ; If a tile is out of place:
                    ( when ( not ( eq ( nth i state ) ( nth i goal ) ) )

                        ; Find the correct position
                        ( setf correct-pos ( position ( nth i state ) goal ) )

                        ; Increment count by the number of rows off
                        ( setf count ( + count
                            ( abs ( -
                                ( floor i puz-size )
                                ( floor correct-pos puz-size )
                            ) )
                        ) )

                        ; Increment count by the number of columns off
                        ( setf count ( + count
                            ( abs ( -
                                ( mod i puz-size )
                                ( mod correct-pos puz-size )
                            ) )
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
    out of place, moving error tiles as it finds them."
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
                        
                        ; Find the position of the
                        ; tile that should be at this position
                        ( setf fetch-pos ( position ( nth i goal ) lst ) )
                        
                        ; Increment count by the number of rows off
                        ( setf count ( + count
                            ( abs ( -
                                ( floor i puz-size )
                                ( floor fetch-pos puz-size )
                            ) )
                        ) )

                        ; Increment count by the number of columns off
                        ( setf count ( + count
                            ( abs ( -
                                ( mod i puz-size )
                                ( mod fetch-pos puz-size )
                            ) )
                        ) )

                        ; Swaps the tiles
                        ( rotatef ( nth i lst ) ( nth fetch-pos lst ) )
                    )
                )
            )
        )
    )
)

; Heuristic for how close a state is to the goal state based
; on the Manhattan distance of the tiles out of place plus a score
; based on the number of tiles out of place. Corrects tiles
; out of place as it finds them.
( defun count_wrong_w_nilsson_score ( state goal )
    "Counts the Manhattan distance of the tiles
    out of place and the number out of place, moving 
    error tiles as it finds them."
    ( let
        (
            ( lst ( copy-list state ) ) ; Local copy of state
            ( count 0 ) ; The number of tiles out of place
            ( puz-size ( isqrt ( length state ) ) ) ; Side length of the puzzle
            fetch-pos ; The position of a tile to be fetched
        ( nilsson 0 ) ; Nilsson sequence score
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
                        
                        ; Find the position of the
                        ; tile that should be at this position
                        ( setf fetch-pos ( position ( nth i goal ) lst ) )
                        
                        ; Increment count by the number of rows off
                        ( setf count ( + count
                            ( abs ( -
                                ( floor i puz-size )
                                ( floor fetch-pos puz-size )
                            ) )
                        ) )

                        ; Increment count by the number of columns off
                        ( setf count ( + count
                            ( abs ( -
                                ( mod i puz-size )
                                ( mod fetch-pos puz-size )
                            ) )
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
                    ; Checks each tile to see if it is out of place
                    ( when ( not ( eq ( nth i state ) ( nth i goal ) ) )
            ; Adds 2 to nilsson score for non center, 1 if center is off
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
