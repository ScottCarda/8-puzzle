( defun successors ( state )
    ( let (
            ( location ( position 0 state ) )
            ( succ '() )
            UP DOWN LEFT RIGHT
          )
        
        ; Perform UP move and add it to successors
        ( when ( > location 3 )
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
    )
)
