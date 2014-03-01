### sudoku_solver
## Description
A sudoku solver written in scala. The program takes in sudoku puzzle via a text
file and returns the solution to standard out.

## Algorithm
The program iterates over each square continuously. At each square it attempts 
to find a space for each character that is valid uniquely to that character.
If a character is found to be placable only at on position in the square it
is added to the grid.

## Input File Format
The input file must be 9x9. Each number is represented by that number. Each
space is indicated with a single period. A sample input is displayed below:
  
91.7.....

.326.9.8.

..7.8.9..

.86.3.17.

3.......6

.51.2.84.

..9.5.3..

.2.3.149.

.....2.61

## Notes
* This is a work in progress...
* Currently it can only solve easy sudoku puzzles


