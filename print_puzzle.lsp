#|
                    ***** PRINT_PUZZLE.LSP *****

Routine for Printing out 8-puzzle in the following format:
1 2 3
8 0 4
7 6 5

Where 0 is equivalent to "blank"

Author: J. Anthony Brackins
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#


#|--------------------------------------------------------------------------|#
#|                           Statistics Printout                            |#
#|--------------------------------------------------------------------------|#
;Print out the stats pertaining to the algorithm used
( defun print_stats ()
    ;eventually add stats to the parameter
    ;(format t "~S graph search~%" algorithm ) 
    (format t "~%<Whatever> graph search~%"  ) 
    (format t "----------------------------------------------~%")
    ;(format t "Solution found in ~S moves~%" moves ) 
    (format t "Solution found in X moves~%" ) 
    ;(format t "~S nodes generated (~S distinct nodes), ~S nodes expanded~%" n_gen n_distinct n_expanded)
    (format t "31 nodes generated (22 distinct nodes), 10 nodes expanded~%")
)


#|--------------------------------------------------------------------------|#
#|                              Print Puzzles                               |#
#|--------------------------------------------------------------------------|#
;Print out all of the n-puzzle transitions that led to the solution
( defun print_puzzles 
    ( 
        all_puzzles 
        &optional ( n_value 8 ) ( col_size 4 ) 
    )
    
    ;Given a list of puzzle states, print them all out
    (let 
        ( 
            (per_column col_size ) 
            ( per_row 0 )
            ( col 0 ) 
            ( row 0 )
            ( x 0 ) 
            ( y 0) 
            ( count (length all_puzzles) ) 

            ( puz_width  3 )
            ( puz_height 3 )

            ( j 0 )
            ( puz_itr   0 )
        )
        
        ( setf puz_width ( sqrt ( + n_value 1 ) ) )
        ( setf puz_height puz_width )
        ( setf per_row puz_height )
        ;print out statistics of the algorithm
        ;will later have to modify this to handle
        ;inputting the actual stats, currently does
        ;nothing!
        ( print_stats )

        ;col monitors the column total, so there are
        ;<col> states per row.
        ;x is the iterator that ensures total printed 
        ;matches the total number of states
        ;y is the iterator for a given row, as we have to 
        ;backtrack a bunch from the way we're printing stuff
        ;out right now
        ( loop while (< x count) do 
            

            ( setf puz_itr 0 )
            (dotimes (i puz_width t)

                ;formatting block
                ( format t "~%    " )

                ;Reset some iterators 
                ( setq col 0)
                ( setq y x)

                ;Print col_size many puzzles
                ( loop while (and ( < col per_column )( < y count )) do
                    ;Print out some puzzles, why don't ya!
                    ( if (< y (- count 1 ) ) 
                        ;TRUE - Print out an arrow, there are still more puzzles!
                        ( print_slice ( nth y all_puzzles  ) puz_width puz_itr t  )

                        ;FALSE - No More arrows, you're at the end of the list!
                        ( print_slice ( nth y all_puzzles  ) puz_width puz_itr )
                    )
                    ;increment iterators for traversing each block of puzzles
                    ( setq col (+ 1 col))
                    ( setq y (+ 1 y))
                )
                ;increment puz_iterator to get the next row of a given block
                ( setf puz_itr (+ puz_itr per_row))
            )


            ;formatting block, reset iterators
            ( format t "~%" ) 
            ( setq x y)      

        )
        ;format for finishing the script
        ( format t "~%" ) 
    )
    'done
)


#|--------------------------------------------------------------------------|#
#|                              Helper Functions                            |#
#|--------------------------------------------------------------------------|#
;Print out the mid row of a given puzzle, as well as an arrow if needed
( defun print_slice 
    ( 
        puzzle 
        width
        itr
        &optional ( last nil )

    )
    ( let 
        (
            ( start 0 )
            ( stop  0 )
        )

        ( setf start itr )
        ( setf stop ( + itr ( - width 1 ) ) )

        ;(print start)
        ;(print stop)
        ;format puzzle
            ( loop for i from start to stop 
                do 

                ( setf val ( format_char ( nth i puzzle ) ) )
                (format t "~A " val )
            )

            (cond
                ;If this is the last state, don't draw an error
                ( (null last) (format t "      " )  )
                ;Otherwise, do draw another arrow
                ( T           (format t "  ->  " )  ) 
            )
        
    )
)



;Print out a character in the puzzle map with proper formatting
( defun format_char (val)
    ;Zeroes show up as blanks in the puzzle display.
    (cond
        ( (= val 0) 
            (setq x " "))
        (t 
            (setq x val))
    )
)
