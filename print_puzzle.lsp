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

;--------------------------------------------------------------------------

;--------------------------------------------------------------------------


;Print out the stats pertaining to the algorithm used
( defun stat_printer ()
    ;eventually add stats to the parameter
    ;(format t "~S graph search~%" algorithm ) 
    (format t "~%<Whatever> graph search~%"  ) 
    (format t "----------------------------------------------~%")
    ;(format t "Solution found in ~D moves~%" moves ) 
    (format t "Solution found in X moves~%" ) 
    ;(format t "~D nodes generated (~D distinct nodes), ~D nodes expanded~%" n_gen n_distinct n_expanded)
    (format t "31 nodes generated (22 distinct nodes), 10 nodes expanded~%")
)


;Print out all of the puzzle transitions that led to the solution
( defun puzzle_printer ( all_puzzles col_size )
    
    ;Given a list of puzzle states, print them all out
    (let ( (per_column col_size ) (col 0) (x 0 ) (y 0) (count (length all_puzzles) ) )
        
        ;print out statistics of the algorithm
        ;will later have to modify this to handle
        ;inputting the actual stats, currently does
        ;nothing!
        ( stat_printer )

        ;col monitors the column total, so there are
        ;<col> states per row.
        ;x is the iterator that ensures total printed 
        ;matches the total number of states
        ;y is the iterator for a given row, as we have to 
        ;backtrack a bunch from the way we're printing stuff
        ;out right now
        (loop while (< x count) do 
            
            ;formatting block
            (format t "~%    " ) 
            (setq col 0)
            (setq y x)

            ;Print the next four top positions
            (loop while (and ( < col per_column )( < y count )) do
                ;(print x)
                (print_top (nth y all_puzzles))
                (setq col (+ 1 col))
                (setq y (+ 1 y))
            )

            ;formatting block, reset iterators
            (format t "~%    " ) 
            (setq col 0)
            (setq y x)

            ;Print the next four middle positions
            (loop while (and ( < col per_column ) ( < y count )) do
                (cond
                    ;IF this is the last state, don't draw another arrow
                    ( ( eq (nth (- count 1) all_puzzles) (nth y all_puzzles) )   
                        (print_mid (nth y all_puzzles) nil) )
                    ;Otherwise, draw the middle with an arrow
                    ( T     
                        (print_mid (nth y all_puzzles) t) ) 
                )         
                (setq col (+ 1 col))
                (setq y (+ 1 y))
            )   

            ;formatting block, reset iterators
            (format t "~%    " ) 
            (setq col 0)
            (setq y x)

            ;Print the next four bottom positions
            (loop while (and ( < col per_column ) ( < y count )) do
                ;(print x)
                (print_bottom (nth y all_puzzles))
                (setq col (+ 1 col))
                (setq y (+ 1 y))
            )

            ;formatting block, reset iterators
            (format t "~%" ) 
            (setq x y)      

        )
        ;format for finishing the script
        (format t "~%" ) 
    )
)

;Print out the top row of a given puzzle
( defun print_top (puzzle)
    (let ((a (nth 0 puzzle))
          (b (nth 1 puzzle))
          (c (nth 2 puzzle)))
         (format t "~D ~D ~D      " a b c ) 
    )
)

;Print out the mid row of a given puzzle, as well as an arrow if needed
( defun print_mid (puzzle last)
    (let ((a (nth 3 puzzle))
          (b (nth 4 puzzle))
          (c (nth 5 puzzle)))
        (cond
            ;If this is the last state, don't draw an error
            ( (null last) (format t "~D ~D ~D      " a b c )  )
            ;Otherwise, do draw another arrow
            ( T           (format t "~D ~D ~D  ->  " a b c )  ) 
        )
    )
)

;Print out the bottom row of a given puzzle
( defun print_bottom (puzzle)
    (let ((a (nth 6 puzzle))
          (b (nth 7 puzzle))
          (c (nth 8 puzzle)))
         (format t "~D ~D ~D      " a b c ) 
    )
)


;Print out a character in the puzzle map with proper formatting
( defun print_char (val)
    (cond
        ( (= val 0) 
            (format t "~D" '" " ) 
        )
        (t (format t "~D" val ) )
    )
)

( defun main ()
    ( cond 

        ( t 
            (setf number (car *args*))
            (setf p1 (list 1 2 3 4 5 6 7 8 0))
            (setf p2 (list 1 2 3 8 0 4 7 6 5))
            (setf p3 (list 3 2 1 6 5 4 0 8 7))
            (setf p4 (list 1 1 1 1 1 0 1 1 1))
            (setf p5 (list 5 6 7 8 1 2 3 4 0))
            (setf p6 (list 0 0 0 1 2 3 0 0 0))
            (setf puzzles (list p1 p2 p3 p4 p5 p6))

            ;(print puzzles)
            ( puzzle_printer puzzles 4)
        )
    )
)

( main )