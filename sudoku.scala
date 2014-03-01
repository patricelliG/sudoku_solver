/* 
* Name: Sudoku_Solver
* Class: CSC_435
* Date: 02/28/2014
* Written By: Gary Patricelli                      
*/

import Array._
import scala.io.Source

// Globals... 
var rows = ofDim[Char](0,0) // The contents of all rows 
var cols = ofDim[Char](0,0) // The contents of all columns
var sqrs = ofDim[Char](0,0) // The contents of all squares
val charSet = List[Char]('1', '2', '3', '4', '5', '6', '7', '8', '9') // The character bank

def initGrid() {
  // Get file name
  val fileName = readLine("Enter the file name to read from.\n> ").toString
  // Open file
  var lineNum = 0 // Placemark for the file line number
  try { 
    for (line <- Source.fromFile(fileName).getLines()) {
      if (lineNum == 0) {
        // Get size of grid to init rows, cols, sqrs 
        val gridSize = line.length
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
  catch {
    case ex: Exception => println("File not found.")
  }
}

// Prints the grid to std out
def printGrid() {
  for (row <- rows) { 
    for (char <- row) {
      print(char)
    }
    println()
  }
}

// Given a square, a position in the square and a character,
// this method will check its validity against all rules
def isValid(sqrNum: Int, sqrPos: Int, char: Char) : Boolean = {
  // Get row and column position
  val (row, col) = sqrPosToRowCol(sqrNum, sqrPos)
  if (rows(row).contains(char) || cols(col).contains(char) || sqrs(sqrNum).contains(char))
    return false
  else
    return true
}

// Given a square and a position in that square this function
// will convern it to the corresponding "row" and "column"
def sqrPosToRowCol(sqrNum: Int, sqrPos: Int) : (Int, Int)  = {
  // Maps of row and column positions in a 3 x 3 square
  val rowOffset = Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 1, 4 -> 1, 5 -> 1, 6 -> 2, 7 -> 2, 8 -> 2)
  val colOffset = Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 0, 4 -> 1, 5 -> 2, 6 -> 0, 7 -> 1, 8 -> 2)
  // Compute row
  val row = (((sqrNum/3).toInt) * 3) + rowOffset(sqrPos)
  // Compute column
  val col = ((sqrNum % 3) * 3) + colOffset(sqrPos)
  return (row, col) 
}

// This function takes a square, starting position, and list
// it recursively collects the positions of blank spaces in
// the given square. It returns a list of the square postitions
// that are blank spaces.
def getBlankSpaces (sqrNum: Int, startPos: Int, indeces: List[Int]) : List[Int] = {
  val index = sqrs(sqrNum).indexWhere(_ == '.', startPos)
  if (startPos >= sqrs.length || index == -1) {
    // We have reached the end of the possinle square positions
    // return the list of spaces
    return indeces
  }
  else {
    // Get the next space after the current index
    getBlankSpaces (sqrNum, index + 1, indeces :+ index)
  }
}
 
// Simply updates all arrays to include 'char' in square
// 'sqrNum' at position 'sqrPos'
def addToGrid(sqrNum: Int, sqrPos: Int, char: Char) { 
  // Calculate row and column
  val (row, col) = sqrPosToRowCol(sqrNum, sqrPos)
  // Add to all arrays
  rows(row)(col) = char
  cols(col)(row) = char
  sqrs(sqrNum)(sqrPos) = char
}

// This function calculates the total number of valid placements
// of the given char in the given square. If there is only one
// placement, it returns the position of the unique space. If 
// there is more than one valid placement it returns -1 indicating
// that there is more than one valid placement.
def uniquePlacement(sqrNum: Int, char: Char) : Int = {
  // Get list of empty spaces in the square    
  val spaces = getBlankSpaces(sqrNum, 0, List[Int]())
  // For each space, check if char can be placed
  var lastPosition = 0 // Save of last valid position for the char
  var validCount = 0  // Keeps track of how many valid placements there has been
  for (space <- spaces) {
    if (isValid(sqrNum, space, char)) { 
      validCount = validCount + 1
      lastPosition = space
    }
  }
  if (validCount == 1) {
    // There was only one valid placement
    // Return the last known position
    return lastPosition
  }
  else {
    // There was either no good position
    // or more than one unique placement
    return -1
  }
}


// This function will place any chars of
// charset that have only one possible placement
// in the given square.
def placeUniqueChars(sqrNum: Int) {
  for (char <- charSet) {
    val sqrPos = uniquePlacement(sqrNum, char)  
    if (sqrPos != -1)
      addToGrid(sqrNum, sqrPos, char)
  }
}

// Function returns true if puzzle is solved
// It does this by checking all rows for blank
// spaces. No blank spaces = true
def isSolved() : Boolean =  {
  for (row <- rows) {
    if (row.contains('.')) { 
      return false
    }
  }
  return true
}

// This function will solve the sudoku.
// In the event an infinate loop is detected
// the program will exit gracefully.
def solve() {
  var loopCount = 0
  while (!isSolved() && loopCount < 100) {
    for (square <- 0 to 8){
      placeUniqueChars(square)
    }
    loopCount = loopCount + 1
  }
  if (loopCount >= 100)
    println("Unable to solve sudoku, exiting...")
  else 
    printGrid()
}

// I dont know. This feels wrong...
initGrid()
solve()


