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
