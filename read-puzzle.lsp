#|
                    ***** READ-PUZZLE.LSP *****

Routines for reading puzzles into lists. Puzzles returned from
these routines are expressed in row-major order.

Authors: J. Anthony Brackins, Scott Carda, Leif Torgersen
Written Spring 2016 for CSC447/547 AI class.

Modifications:

|#

#|--------------------------------------------------------------------------|#
#|                       Read In Puzzle from CLI                            |#
#|--------------------------------------------------------------------------|#

; Gets a puzzle from user input.
( defun read-puzzle ()
    "Gets a puzzle from user input."
    ( let
        (
            ; The user input as a string
            ( str ( read-line ) )
        )
        ; Pretend the string is a file and pass it to the 
        ( with-input-from-string ( stream str )
            ( get-puzzle stream )
        )
    )
)

#|--------------------------------------------------------------------------|#
#|                      Read In Puzzle from File                            |#
#|--------------------------------------------------------------------------|#

; Reads a puzzle in from a file.
( defun read-puzzle-file ( filename )
    "Reads a puzzle from a file."
    ( let
        (
            ( file ( open filename ) ) ; The file stream
            puzzlelist    ; The puzzle list to be returned
        )

        ; If the file was successfully opened
        ( when file
            ; Uses the stream-reading function to get the puzzle list
            ( setf puzzlelist ( get-puzzle file ) )
            ( close file )
            ; Returns the puzzle list
            puzzlelist
        )
    )
)

; Recursively reads a puzzle from an input stream.
( defun get-puzzle ( file )
    "Reads a puzzle from an input stream."
    ( let
        (
            ; Reads the next input from the file
            ( input ( read file NIL NIL ) )
        )

        ( cond
            ; If there is no more input ( base case )
            ( ( not input )
                NIL
            )

            ; Else recurses
            ( t
                ( cons input ( get-puzzle file ) )
            )
        )
    )
)

