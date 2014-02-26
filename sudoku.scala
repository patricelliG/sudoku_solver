import Array._
import scala.io.Source


// Function to initialize sodoku from file
def initBoard(filename: String) : Array[Array[Char]] = {
  
  // Open file
  var lineNum = 0
  var board = ofDim[Char](0,0)
  for (line <- Source.fromFile(filename).getLines()) {
      // Init board if first line 
      if (lineNum == 0) {
        board = ofDim[Char](line.length, line.length)
        println("Initializing Board...")
      }
      //Parse this line of the file
      parseInput(lineNum, line, board) 

      lineNum = lineNum + 1
    }
    return board
}

// Function to parse a single line from a file into the board
def parseInput(lineNum: Int, line: String,  board: Array[Array[Char]]){
  var index = 0
  while (index < line.length) {
    if (line.charAt(index) == '.') {
      // This is a blank space, we denote it with -1
      board(lineNum)(index) = 'x' 
    }
    else {
      board(lineNum)(index) = (line.charAt(index))
    }
    index = index + 1
  }
}

// Function to print board to standard out
def printBoard(board: Array[Array[Char]]) {
  var rowIndex = 0
  while (rowIndex < board.length) {
    var columnIndex = 0
    while (columnIndex < board.length) {
      if (board(rowIndex)(columnIndex) == 'x'){
        print(" ")
      }
      else {
        print(board(rowIndex)(columnIndex))
      }
      columnIndex = columnIndex + 1
    }
    println()
    rowIndex = rowIndex + 1
  }
}

// Function that returns a square, given a grid and square number
//def getSquare(grid: Array[Array[Char]], squareNum: Int) : Array[Array[Char]] = { 
//}

// Function that checks if 'char' is present in the row
def checkRow(grid: Array[Array[Char]], rowNum: Int, checkChar: Char) : Boolean = {
  var colIndex = 0
  while (colIndex < grid.length) { 
    if (grid(rowNum)(colIndex) == checkChar) { 
      return false
    }
    colIndex = colIndex + 1
  }
  return true
}

// Function that checks if 'char' is present in the column 
def checkCol(grid: Array[Array[Char]], colNum: Int, checkChar: Char) : Boolean = {
  var rowIndex = 0
  while (rowIndex < grid.length) {
    if (grid(rowIndex)(colNum) == checkChar) {
      return false
    }
    rowIndex = rowIndex + 1
  }
  return true 
}


// printBoard(initBoard("input1.txt"))

