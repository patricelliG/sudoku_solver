import Array._

var rows = ofDim[Char](0,0) // The contents of all rows 
var cols = ofDim[Char](0,0) // The contents of all columns
var sqrs = ofDim[Char](0,0) // The contents of all squares

def initGrid(filename: String) {
  // Open file

  var lineNum = 0
  for (line <- Source.fromFile(filename).getLines()) {
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
    for (colIndex <- 0 to line.length) {
      cols(colIndex)(lineNum) = line.charAt(colIndex)
    }
  }
  // Now to initialize the squares


}


// This function takes a sqaure number and position
// It returns the coresponding row and column for that position
def sqrPosToRowCol(sqrNum: Int, sqrPos: Int) : (Int, Int) { 
  // Maps of row and column positions in a 3 x 3 square
  val rowOffset = Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 1, 4 -> 1, 5 -> 1, 6 -> 2, 7 -> 2, 8 -> 2)
  val colOffset = Map(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 0, 4 -> 1, 5 -> 2, 6 -> 0, 7 -> 1, 8 -> 2)

  val row = (((sqrNum/3).toInt) * 3) + rowOffset(sqrPos)
  val col = (((sqrNum % 3) * 3) + colOffset(sqrPos)

  return (row, col)
}
