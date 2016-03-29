# 8-puzzle

#8-Puzzle Description
In the study of Artificial Intelligence, the 8-puzzle is a 
simple sliding puzzle "toy" problem used to illustrate the 
concepts of search space. To solve this puzzle, 8 tiles are 
repositioned about a 3x3 grid in a sliding fashion in order 
to acheive a goal state. These puzzles are represented in 
row-major form, with the tiles being represented as 1-8, with 
the space being represented as a 0. A standard 8-puzzle game is 
simulated below:

    1 3 4       1 3 4       1 3 4       1 3         
    8 6 2   ->  8   2   ->  8 2     ->  8 2 4   ->  
    7   5       7 6 5       7 6 5       7 6 5       

    1   3       1 2 3       
    8 2 4   ->  8   4   <- (This is the goal state!)    
    7 6 5       7 6 5       

#Program Objective
The objective of this assignment is to use the Lisp programming 
language to solve the 8-puzzle using Breadth-First Search (BFS), 
Depth First Iterated Deepening (DFID), and A*, a heuristics-based 
search method.

#BFS Strategy
Breadth-First Search is a standard algorithm for searching graph 
structures. From a given start state, this algorithm searches 
neighboring nodes of the same level first, before exploring 
neighbors present in the next level.

          (1)
         / | \
       (2)(3)(4)
       /   |  | \
     (5)  (6)(7)(8)
      |
     (9)
     
     
#DFID Strategy
Depth First Iterated Deepening is a state spaace search strategy 
in which there is a depth limit to the Depth-First Search (DFS) 
with increasing depth limits until a goal state is reached. This 
allows for a version of depth-first search similar to BFS but with 
much smaller memory requirements. 

#A* Search Strategy
The A* search algorithm is a version of Dijkstra's algorithm that 
performs better than exhaustive searches in certain situations due to 
its use of heuristics to guide search.
As the search algorithm is running, A* determines the next node to 
expand by determining the estimate of the cost or weight to reach 
the goal state. This is done by using the following equation:

    f(n) = g(n) + h(n)

Where n is the node on the path, g(n) is the cost from the start 
node to the given n, and h(n) is the heuristic value that estimates 
the remaining cost from n to the goal state.

#Heuristics
Heuristics can be both admissible and inadmissible. Admissable 
heuristics never overestimate the cost to reach the goal, allowing 
admissible heuristics to find the shortest path. This program features 
both inadmissible and admissible heuristics, and the summary statistics 
printed during the program's runtime will indicate whether an algorithm 
is using an inadmissible or admissible heuristic, as well as a brief 
summary of the heuristic itself. BFS and DFID are exhaustive search 
techniques, and as a result do not use heuristics at all.

#Program Usage
To run our program, a user must provide the start position of the puzzle.


This can be specified in a puzzle file, within the Lisp interpreter by 
passing a list to the 8puzzle function call, or interactively by calling 
the 8puzzle function without a start state list.

The puzzlefile contains an 8-puzzle start position, consisting of 9 digits 
separated by white space, in row-major order. The digits 1-8 represent 
the 8 tiles, and 0 represents the blank.

An example of a goal state is the following puzzle configuration:

    1 2 3       
    8 0 4 
    7 6 5

This configuration will be represented in the CLISP program as the 
following list:

    ( 1 2 3 8 0 4 7 6 5 )
    
------------------------------------------------------------

#Program Usage Example: Running Program From Command Line

easy.puz file:

    1 3 4 
    8 6 2 
    7 0 5

Command Line: 

    clisp 8puzzle.lsp puzzlefile

------------------------------------------------------------

#Program Usage Example: CLISP, passing start state as list

Inside CLISP:

    ( load '8puzzle )
    ( 8puzzle '( 1 3 4 8 6 2 7 0 5 ) )

------------------------------------------------------------

#Program Usage Example: CLISP without passing in start state

Inside CLISP:

    ( load '8puzzle )
    ( 8puzzle )
    Please enter a puzzle:
    >> 1 3 4 8 6 2 7 0 5

------------------------------------------------------------

#Authors
J. Anthony Brackins, Scott Carda, Leif Torgersen

#Course
Written Spring 2016 for CSC447/547 AI class.

#Modifications
For Additional Credit, the program has been expanded beyond the 
standard 8-puzzle to handle N-puzzles, where N may be:

* 3<sup>2</sup> - 1 = 8 (standard 8-puzzle)
* 4<sup>2</sup> - 1 = 15-puzzle
* 5<sup>2</sup> - 1 = 24-puzzle
* etc.

The program has been scaled up so that the program will be able to 
generate the goal state of any given puzzle and determine the puzzle 
size based on the size of the list read in as the initial puzzle state,
as long as the initial puzzle given to the program is in a valid 
N-puzzle format.

#Solving Tough.puz

Our program can solve tough.puz with all of our algorithms. However, BFS 
and DFID both take a very long time, so we've included their printed out 
runs to verify that they still worked. 
BFS started at  3:41PM and ended at 4:17PM (approximately 36 minutes) and 
DFID started at 4:17PM and ended at 4:38PM (approximately 21 minutes)
All A* runs were almost instantaneous.

Tough.puz does a great job of demonstrating how our program's inadmissible 
heuristic, while fast, can yield a suboptimal answer. The optimal answer 
appears to be the 18 move solution, whereas using the Count Manhattan Distance 
of Incorrect Elements and add Nilsson sequence score heuristic yields a 
solution in 20 moves.
        
    BFS graph search 
    ---------------------------------------------------------
    Solution found in 18 moves
    76856 nodes generated (40592 distinct nodes), 28620 nodes expanded

    2 1 3       2 1 3       2 1 3       2 1 3       
    8   4   ->  8 7 4   ->  8 7 4   ->  8 7     ->  
    6 7 5       6   5       6 5         6 5 4       

    2 1         2   1         2 1       8 2 1       
    8 7 3   ->  8 7 3   ->  8 7 3   ->    7 3   ->  
    6 5 4       6 5 4       6 5 4       6 5 4       

    8 2 1       8   1       8 1         8 1 3       
    7   3   ->  7 2 3   ->  7 2 3   ->  7 2     ->  
    6 5 4       6 5 4       6 5 4       6 5 4       

    8 1 3       8 1 3       8 1 3       8 1 3       
    7 2 4   ->  7 2 4   ->  7 2 4   ->    2 4   ->  
    6 5         6   5         6 5       7 6 5       

      1 3       1   3       1 2 3       
    8 2 4   ->  8 2 4   ->  8   4       
    7 6 5       7 6 5       7 6 5       


    DFID graph search 
    ---------------------------------------------------------
    Solution found in 18 moves
    211233 nodes generated (77401 distinct nodes), 48950 nodes expanded

    2 1 3       2   3         2 3       8 2 3       
    8   4   ->  8 1 4   ->  8 1 4   ->    1 4   ->  
    6 7 5       6 7 5       6 7 5       6 7 5       

    8 2 3       8 2 3       8 2 3       8 2 3       
    6 1 4   ->  6 1 4   ->  6 1 4   ->  6 1     ->  
      7 5       7   5       7 5         7 5 4       

    8 2         8   2       8 1 2       8 1 2       
    6 1 3   ->  6 1 3   ->  6   3   ->    6 3   ->  
    7 5 4       7 5 4       7 5 4       7 5 4       

      1 2       1   2       1 2         1 2 3       
    8 6 3   ->  8 6 3   ->  8 6 3   ->  8 6     ->  
    7 5 4       7 5 4       7 5 4       7 5 4       

    1 2 3       1 2 3       1 2 3       
    8 6 4   ->  8 6 4   ->  8   4       
    7 5         7   5       7 6 5       


    A* graph search ( heuristic: Count Incorrect Elements ( Admissible ) )
    ---------------------------------------------------------
    Solution found in 18 moves
    14763 nodes generated (8715 distinct nodes), 5287 nodes expanded

    2 1 3       2   3         2 3       8 2 3       
    8   4   ->  8 1 4   ->  8 1 4   ->    1 4   ->  
    6 7 5       6 7 5       6 7 5       6 7 5       

    8 2 3       8 2 3       8 2 3       8 2 3       
    6 1 4   ->  6 1 4   ->  6 1 4   ->  6 1     ->  
      7 5       7   5       7 5         7 5 4       

    8 2         8   2       8 1 2       8 1 2       
    6 1 3   ->  6 1 3   ->  6   3   ->    6 3   ->  
    7 5 4       7 5 4       7 5 4       7 5 4       

      1 2       1   2       1 2         1 2 3       
    8 6 3   ->  8 6 3   ->  8 6 3   ->  8 6     ->  
    7 5 4       7 5 4       7 5 4       7 5 4       

    1 2 3       1 2 3       1 2 3       
    8 6 4   ->  8 6 4   ->  8   4       
    7 5         7   5       7 6 5       


    A* graph search ( heuristic: Count Manhattan Distance of 
    Incorrect Elements ( Admissible ) )
    ---------------------------------------------------------
    Solution found in 18 moves
    2837 nodes generated (1713 distinct nodes), 1009 nodes expanded

    2 1 3       2   3         2 3       8 2 3       
    8   4   ->  8 1 4   ->  8 1 4   ->    1 4   ->  
    6 7 5       6 7 5       6 7 5       6 7 5       

    8 2 3       8 2 3       8 2 3       8 2 3       
    6 1 4   ->  6 1 4   ->  6 1 4   ->  6 1     ->  
      7 5       7   5       7 5         7 5 4       

    8 2         8   2       8 1 2       8 1 2       
    6 1 3   ->  6 1 3   ->  6   3   ->    6 3   ->  
    7 5 4       7 5 4       7 5 4       7 5 4       

      1 2       1   2       1 2         1 2 3       
    8 6 3   ->  8 6 3   ->  8 6 3   ->  8 6     ->  
    7 5 4       7 5 4       7 5 4       7 5 4       

    1 2 3       1 2 3       1 2 3       
    8 6 4   ->  8 6 4   ->  8   4       
    7 5         7   5       7 6 5       


    A* graph search ( heuristic: Count Manhattan Distance of Incorrect Elements 
    and add Nilsson sequence score ( Inadmissible ) )
    ---------------------------------------------------------
    Solution found in 20 moves
    2645 nodes generated (1581 distinct nodes), 932 nodes expanded

    2 1 3       2 1 3       2 1 3       2 1 3       
    8   4   ->  8 7 4   ->  8 7 4   ->    7 4   ->  
    6 7 5       6   5         6 5       8 6 5       

      1 3       1   3       1 7 3       1 7 3       
    2 7 4   ->  2 7 4   ->  2   4   ->    2 4   ->  
    8 6 5       8 6 5       8 6 5       8 6 5       

    1 7 3       1 7 3       1 7 3       1   3       
    8 2 4   ->  8 2 4   ->  8   4   ->  8 7 4   ->  
      6 5       6   5       6 2 5       6 2 5       

      1 3       8 1 3       8 1 3       8 1 3       
    8 7 4   ->    7 4   ->  7   4   ->  7 2 4   ->  
    6 2 5       6 2 5       6 2 5       6   5       

    8 1 3       8 1 3         1 3       1   3       
    7 2 4   ->    2 4   ->  8 2 4   ->  8 2 4   ->  
      6 5       7 6 5       7 6 5       7 6 5       

    1 2 3       
    8   4       
    7 6 5     

#Solving Worst.puz

Our inadmissable heuristic for A* solves the worst.puz with the following output:

    A* graph search ( heuristic: Count Manhattan Distance of Incorrect Elements and 
    add Nilsson sequence score ( Inadmissible ) )
    ---------------------------------------------------------
    Solution found in 32 moves
    29578 nodes generated (17010 distinct nodes), 10567 nodes expanded

    5 6 7       5 6 7       5 6 7       5 6 7       
    4   8   ->  4 8     ->  4 8 1   ->  4 8 1   ->  
    3 2 1       3 2 1       3 2         3   2       

    5 6 7       5 6 7       5 6         5   6       
    4   1   ->  4 1     ->  4 1 7   ->  4 1 7   ->  
    3 8 2       3 8 2       3 8 2       3 8 2       

    5 1 6       5 1 6       5 1 6       5 1 6       
    4   7   ->    4 7   ->  3 4 7   ->  3 4 7   ->  
    3 8 2       3 8 2         8 2       8   2       

    5 1 6       5 1 6       5 1 6       5 1 6       
    3 4 7   ->  3 4     ->  3   4   ->    3 4   ->  
    8 2         8 2 7       8 2 7       8 2 7       

      1 6       1   6       1 3 6       1 3 6       
    5 3 4   ->  5 3 4   ->  5   4   ->  5 2 4   ->  
    8 2 7       8 2 7       8 2 7       8   7       

    1 3 6       1 3 6       1 3         1   3       
    5 2 4   ->  5 2     ->  5 2 6   ->  5 2 6   ->  
    8 7         8 7 4       8 7 4       8 7 4       

    1 2 3       1 2 3       1 2 3       1 2 3       
    5   6   ->    5 6   ->  8 5 6   ->  8 5 6   ->  
    8 7 4       8 7 4         7 4       7   4       

    1 2 3       1 2 3       1 2 3       1 2 3       
    8   6   ->  8 6     ->  8 6 4   ->  8 6 4   ->  
    7 5 4       7 5 4       7 5         7   5       

    1 2 3       
    8   4       
    7 6 5
    
Due to the fact that worst.puz is not solvable with our other algorithms,
we included this here to show that one of our algorithm was able to solve it.

