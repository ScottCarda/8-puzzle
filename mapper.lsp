#|
                 ***** MAPPER.LSP *****

Routines for mapping 1-d list into n-puzzle structure

Author: J. Anthony Brackins
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#

( defun mapper 
	( 
	  &optional ( puzzle nil ) (n_value 8) 
  	)

    ( let 
    	(
            ;8 puzzle is 3x3, 15 puzzle is 4x4, etc...
            ( dimension 0 )
            
            ;height and width will start out as
            ;puzzle dimension value
            ( height 0 )
            ( width  0 )
            ( itr    1 )
            ( cursor -1 )
      	)

        ( setf dimension ( sqrt ( + n_value 1 ) ) )
        ( setf height dimension )
        ( setf width  dimension )
        ;( format t "dimension: ~d height: ~d width: ~d~%" dimension height width)
        ( loop do 

            ;( format t "dimension: ~d height: ~d width: ~d~%" dimension height width)

            ;Move cursor over 1.
            ;Seems weird, but we need to start each
            ;"spiral" with moving the cursor to the
            ;right by 1, so that's why the cursor starts
            ;at negative 1
            ( setf cursor ( + cursor 1  ) )

            ;moving LEFT-TO-RIGHT

                ( dotimes ( i width )
                    ;perform swap
                    ;( rotatef ( nth location RIGHT ) ( nth ( + location 1 ) RIGHT ) )
                    ( if ( <= itr n_value )
                        ( setf ( nth cursor puzzle ) itr )
                    )
                    ;increment iterator
                    ( setf itr    ( + itr 1    ) )

                    ;( if ( < i width )
                        ;move over to the right by 1
                        ( setf cursor ( + cursor 1 ) )
                    ;)
                )



            ;height should now be one less
            ( setf height ( - height 1 ) )

            ;Back cursor up for readjustment (we overstepped earlier)
            ( setf cursor ( - cursor 1 ) ) 

            ;Move cursor down once so we don't repeat an index
            ( setf cursor ( + cursor dimension ) )


            ;moving TOP-TO-BOTTOM

                ( dotimes ( i height )
                    ;perform swap
                    ;( rotatef ( nth location RIGHT ) ( nth ( + location 1 ) RIGHT ) )
                    ( if ( <= itr n_value )
                        ( setf ( nth cursor puzzle ) itr )
                    )                    

                    ;increment iterator
                    ( setf itr    ( + itr 1    ) )
                    
                    ;move cursor down by (dimension value)
                    ;( if ( < i height )
                        ( setf cursor ( + cursor dimension ) )
                    ;)
                )



            ;width should now be one less
            ( setf width ( - width 1 ) )

            ;Back cursor up for readjustment (we overstepped earlier)
            ( setf cursor ( - cursor dimension ) ) 
            
            ;Move cursor left once so we don't repeat an index
            ( setf cursor ( - cursor 1 ) )

            ;moving RIGHT-TO-LEFT
 
                ( dotimes ( i width )
                    ;perform swap
                    ;( rotatef ( nth location RIGHT ) ( nth ( + location 1 ) RIGHT ) )
                    ( if ( <= itr n_value )
                        ( setf ( nth cursor puzzle ) itr )
                    )                    
                    ;increment iterator
                    ( setf itr    ( + itr 1    ) )
                    
                    ;move cursor left by 1
                    ;( if ( < i width )
                        ( setf cursor ( - cursor 1 ) )
                    ;)
                )


            ;height should now be one less
            ( setf height ( - height 1 ) )


            ;Back cursor up for readjustment (we overstepped earlier)
            ( setf cursor ( + cursor 1 ) ) 
            
            ;Move cursor up once so we don't repeat an index
            ( setf cursor ( - cursor dimension ) )


                ;moving BOTTOM-TO-TOP
                ( dotimes ( i height )
                    ;perform swap
                    ;( rotatef ( nth location RIGHT ) ( nth ( + location 1 ) RIGHT ) )
                    ( if ( <= itr n_value )
                        ( setf ( nth cursor puzzle ) itr )
                    )                    
                    ;increment iterator
                    ( setf itr    ( + itr 1    ) )
                    
                    ;move cursor down by (dimension value)
                    ;( if ( < i height )

                        ( setf cursor ( - cursor dimension ) )
                    ;)
                )


            ;Back cursor up for readjustment (we overstepped earlier)
            ( setf cursor ( + cursor dimension ) ) 
            

            ;width should now be one less
            ( setf width ( - width 1 ) )

            while 
                ( and 
                    ( <  itr ( + n_value 1 ) )
                    ( >  width  0 )
                    ( >  height 0 ) 
                )
        )

            ;set the 0 now!!
            ;( setf ( nth cursor puzzle ) 0 )
        puzzle
    )


)


( defun generate_goal
    (
        n_value
    )



    ( setf goal_list
        ( make-list ( + 1 n_value ) :initial-element 0 )
    )
    ( setf goal_list
        ( mapper goal_list n_value )
    )
    
    goal_list
)