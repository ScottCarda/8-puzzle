#|
                    ***** PRINT-PUZZLE.LSP *****

Routine for Printing out 8-puzzle in the following format:
1 2 3
8 0 4
7 6 5

Where 0 is equivalent to "blank"

With this strategy, a puzzle sequence will be printed out
like this:

    1 3 4       1 3 4       1 3 4       1 3         
    8 6 2   ->  8   2   ->  8 2     ->  8 2 4   ->  
    7   5       7 6 5       7 6 5       7 6 5       

    1   3       1 2 3       
    8 2 4   ->  8   4       
    7 6 5       7 6 5       

The program can be modified so that more than 4 states are 
printed per line, but that is the current default.

Author: J. Anthony Brackins
Written Spring 2016 for CSC447/547 AI class.

Modifications:
Rewrote this program to handle printing N-puzzles as well as 
the 8-puzzle. It should be stressed that this was an incredible 
pain to pull off. It should also be noted that for puzzles with 
even-dimensions, such as the 4x4 15 puzzle, the arrows are 
off-center, like this: 

 1  2  3  4        1  2  3  4        1  2  3  4        1  2  3  4       
12 14 15  5   ->  12 14 15  5   ->  12 14 15  5   ->  12 14     5   ->  
11  9 13  6       11    13  6       11 13     6       11 13 15  6       
10     8  7       10  9  8  7       10  9  8  7       10  9  8  7       

 1  2  3  4        1  2  3  4       
12    14  5   ->  12 13 14  5       
11 13 15  6       11    15  6       
10  9  8  7       10  9  8  7       


|#

#|--------------------------------------------------------------------------|#
#|                           Statistics Printout                            |#
#|--------------------------------------------------------------------------|#
;Print out the stats pertaining to the algorithm used
( defun print_stats 
    (
        puzzle
        search-type
        &optional ( heuristic nil )
    )
    "Print out statistics pertaining to the search algorithm used"
    ( let 
        (
            ( moves ( - (length puzzle) 1 ) )
        )
        
        ;eventually add stats to the parameter
        (format t "~%~A graph search " search-type ) 
        ( if ( null heuristic ) 
            ;TRUE
            ( format t "~%" )
            
            ;FALSE
            ( format t "( heuristic: ~A )~%" heuristic )
        )

        ( format t 
            "---------------------------------------------------------~%" 
        )
        ( format t "Solution found in ~A moves~%" moves ) 
        ( format t "~A nodes generated "   *generated* )
        ( format t "(~A distinct nodes), " *distinct* )
        ( format t "~A nodes expanded~%"   *expanded* )
    )
)

#|--------------------------------------------------------------------------|#
#|                              Print Puzzles                               |#
#|--------------------------------------------------------------------------|#
;Print out all of the n-puzzle transitions that led to the solution
( defun print_puzzle
    ( 
        all_puzzles 
        &optional ( n_value 8 ) ( col_size 4 ) 
    )
    "Print every state transition that led to the solution state"
    
    ;Given a list of puzzle states, print them all out
    (let 
        ( 
            ;values per row and column
            ( per_column col_size ) 
            ( per_row   0         )

            ;row and column iterators
            ( col 0 ) 
            ( row 0 )

            ;counter variables used for keeping track of position
            ( x 0 ) 
            ( y 0 ) 
            ( count (length all_puzzles) ) 

            ( puz_width  3 )
            ( puz_height 3 )

            ( j 0 )
            ( puz_itr   0 )
        )
        
        ;Set remaining variables
        ( setf puz_width ( sqrt ( + n_value 1 ) ) )
        ( setf puz_height puz_width )
        ( setf per_row puz_height   )


  

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
                ( setq col 0 )
                ( setq y   x )

                ;Print col_size many puzzles
                ( loop while ( and ( < col per_column )( < y count ) ) do
                    ;Print out some puzzles, why don't ya!
                    ;Check to see if you have any more puzzles to print
                    ( if ( < y (- count 1 ) ) 

                        ;TRUE - Print out an arrow, 
                        ;there are still more puzzles!
                        ;Also check that the given slice is 
                        ;in the "middle" of the puzzle so that it 
                        ;properly only prints out one arrow.
                        ( if 
                            ( and ( > 
                                    puz_itr 
                                    ( - (floor n_value 2) puz_width  ) 
                                  ) 
                                  ( < 
                                    puz_itr 
                                    (floor n_value 2) ) 
                            )
                             
                            ;TRUE: center slice, so print the arrow
                            ( print_slice 
                                ( nth y all_puzzles  ) 
                                puz_width puz_itr 
                                t 
                            )
                            ;FALSE: not the center slice, no arror needed
                            ( print_slice 
                                ( nth y all_puzzles ) 
                                puz_width puz_itr 
                            )
                        )

                        ;FALSE - No More arrows, you're at the end of the list!
                        ( print_slice 
                            ( nth y all_puzzles ) 
                            puz_width puz_itr 
                        )
                    )

                    ;increment iterators for traversing each block of puzzles
                    ( setq col ( + 1 col ) )
                    ( setq y   ( + 1 y   ) )
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
    (values)
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
    "Print out a specific row of a given puzzle, as well as an arrow if needed"
    ( let 
        (
            ( start 0 )
            ( stop  0 )
            ( val nil )
            ( plength ( length puzzle ) )
        )

        ( setf start itr )
        ( setf stop ( + itr ( - width 1 ) ) )

        ;(print start)
        ;(print stop)
        ;format puzzle
        ( loop for i from start to stop 
            do 

            ( setf val ( nth i puzzle ) )
            ( cond 
                ;If you're doing a puzzle larger than 8-puzzle, You'll
                ;have to add additional spacing for single digit values 
                ;to balance out the print margins. You'd technically have to 
                ;do this for 3 spaces once you got to the 120 puzzle, but UGH
                ;why would you EVER...
                ( ( and ( > plength 9 ) ( < val 10  ) )     
                    ( setf val ( format_char ( nth i puzzle ) ) )
                    (format t " ~A " val )
                )
                ( t
                    ( setf val ( format_char ( nth i puzzle ) ) )
                    (format t "~A " val )
                )
            )

        )

        (cond
            ;If this is the last state, don't draw an error
            ( (null last) ( format t "      " )  )
            ;Otherwise, do draw another arrow
            ( T           ( format t "  ->  " )  ) 
        )
        
    )
)



;Print out a character in the puzzle map with proper formatting
( defun format_char ( val )
    "Format printed out character, if it's a zero change it to a blank"
    ( let
        (
            (x nil)
        )
        ;Zeroes show up as blanks in the puzzle display.
        (cond
            ( (= val 0) 
                (setq x " "))
            (t 
                (setq x val))
        )
        x
    )
)
