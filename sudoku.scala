/* Vocabulary:
*     Grid: the entire sudoku board 
*     Square: one of the 9 partitioned sections of the grid 
*     Square Position: Each square has 9 cells, 0 - 8
*                       
*/

import Array._
import scala.io.Source

var rows = ofDim[Char](0,0) // The contents of all rows 
var cols = ofDim[Char](0,0) // The contents of all columns
var sqrs = ofDim[Char](0,0) // The contents of all squares
val charSet = List[Char]( '0', '1', '2', '3', '4', '5', '6', '7', '8', '9')

def initGrid(filename: String) {
  // Open file
  var lineNum = 0
  for (line <- Source.fromFile(filename).getLines()) {
    if (lineNum == 0) {
      // Get size of grid to init rows, cols, sqrs 
      val gridSize = line.length
      println("Initializing Arrays...")
      rows = ofDim[Char](gridSize, gridSize)
      cols = ofDim[Char](gridSize, gridSize)
      sqrs = ofDim[Char](gridSize, gridSize)
    }
    // Save this line to the corresponding row 
    rows(lineNum) = line.toCharArray
    // Save this line to the top of each column
    for (colIndex <- 0 to line.length - 1) {
      cols(colIndex)(lineNum) = line.charAt(colIndex)
    }
    lineNum = lineNum + 1
  }
  // Now to initialize the squares
  // This must be done iteratively as the index of the elements must be known
  for (square <- 0 to 8) {
    for (position <- 0 to 8) {
      val (row, col) = sqrPosToRowCol(square, position)
      sqrs(square)(position) = rows(row)(col) 
    }
  }
}

def printGrid() {
  //for (row <- rows) println() for (char <- row) print(char)
  for (row <- rows) { 
    for (char <- row) {
      print(char)
    }
    println()
  }
}

// Given a square, a position in the square and a character,
// this method will check its valiity against all rules
def isValid(sqrNum: Int, sqrPos: Int, char: Char) : Boolean = {
  // Get row and column position
  val (row, col) = sqrPosToRowCol(sqrNum, sqrPos)
  if (rows(row).contains(char) || cols(col).contains(char) || sqrs(sqrNum).contains(char))
    false
  else
    true
}

// This function takes a square number and position
// It returns the coresponding row and column for that position
def sqrPosToRowCol(sqrNum: Int, sqrPos: Int) : (Int, Int)  = {
  // Maps of row and column positions in a 3 x 3 square
  val rowOffset = Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 1, 4 -> 1, 5 -> 1, 6 -> 2, 7 -> 2, 8 -> 2)
  val colOffset = Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 0, 4 -> 1, 5 -> 2, 6 -> 0, 7 -> 1, 8 -> 2)
  val row = (((sqrNum/3).toInt) * 3) + rowOffset(sqrPos)
  val col = ((sqrNum % 3) * 3) + colOffset(sqrPos)
  return (row, col) 
}

// This function takes a square, starting position, and list
// It returns the indices of the remaining blanks of a square
def getBlankSpaces (sqrNum: Int, startPos: Int, indeces: List[Int]) : List[Int] = {
  val index = sqrs(sqrNum).indexWhere(_ == '.', startPos)
  if (startPos >= sqrs.length || index == -1) {
    return indeces
  }
  else {
    getBlankSpaces (sqrNum, index + 1, indeces :+ index)
  }
}
 
def addToGrid(sqrNum: Int, sqrPos: Int, char: Char) { 
  // Calculate row and column
  val (row, col) = sqrPosToRowCol(sqrNum, sqrPos)
  // Add char to rows array
  rows(row)(col) = char
  // Add char to colums array
  cols(col)(row) = char
  // Add to squares array
  sqrs(sqrNum)(sqrPos) = char
}

// This function calculates the total number of valid placements
// of the given char in the given square. If there is only one
// placement, it returns the position of the unique space. If 
// there is more than one valid placement it returns -1.
def numValidSpaces(sqrNum: Int, char: Char) : Int = {
  // Get list of empty spaces in the square    
  val spaces = getBlankSpaces(sqrNum, 0, List[Int]())
  // For each space, check if char can be placed
  var lastPosition = 0
  var validCount = 0 
  for (space <- spaces) {
    if (isValid(sqrNum, space, char)) { 
      validCount = validCount + 1
      lastPosition = space
    }
  }
  if (validCount == 1)
    return lastPosition
  else
    return -1
}



initGrid("input1.txt")
printGrid()
println(numValidSpaces(0, '9'))
