( let
    (
        A
        B
        C
    )
    
    ( defun eval_lst ( x )
        ( setf A 0 )
        ( setf B 0 )
        ( setf C 0 )
        ( dolist ( i x )
        
            ( when ( evenp i )
                ( setf A ( 1+ A ) )
            )
            
            ( when ( = ( mod i 3 ) 0 )
                ( setf B ( 1+ B ) )
            )
            
            ( when ( = ( mod i 5 ) 0 )
                ( setf C ( 1+ C ) )
            )
            
        )
        
        ( list A B C )
        
    )
    
    ( defun getA ()
        A
    )
    
    ( defun getB ()
        B
    )
    
    ( defun getC ()
        C
    )
)
