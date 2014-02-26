import Array._
import scala.io.Source


// Function to initialize sodoku from file
def initBoard(filename: String) : Array[Array[Int]] = {
  
  // Open file
  var lineNum = 0
  var board = ofDim[Int](0,0)
  for (line <- Source.fromFile(filename).getLines()) {
      // Init board if first line 
      if (lineNum == 0) {
        board = ofDim[Int](line.length, line.length)
        println("Initializing Board...")
      }
      //Parse this line of the file
      parseInput(lineNum, line, board) 

      lineNum = lineNum + 1
    }
    return board
}

// Function to parse a single line from a file into the board
def parseInput(lineNum: Int, line: String,  board: Array[Array[Int]]){
  var index = 0
  while (index < line.length) {
    if (line.charAt(index) == '.') {
      // This is a blank space, we denote it with -1
      board(lineNum)(index) = -1 
    }
    else {
      println(line.charAt(index))
      board(lineNum)(index) = (line.charAt(index))
    }
    index = index + 1
  }
}

// Function to print board to standard out
def printBoard(board: Array[Array[Int]]) {
  var rowIndex = 0
  while (rowIndex < board.length) {
    var columnIndex = 0
    while (columnIndex < board.length) {
      if (board(rowIndex)(columnIndex) == -1){
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



printBoard(initBoard("input1.txt"))

