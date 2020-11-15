package bsq

case class Board(board: Array[String]) {

  def isValidBoard(): Boolean = {
    def isValidLine(expectedLength: Int, line: String): Boolean = {
      if (expectedLength != line.length())
        false
      else if (line.exists(char => char != 'o' && char != '.' && char != '\n'))
        false
      else true
    }

    val boardSize = this.board.size
    val expectedLen = if (boardSize == 0) 0 else this.board(0).length()

    this.board.filter(str => isValidLine(expectedLen, str)).length == boardSize
  }

  def toIntBoard(): Array[Array[Int]] = {
    def toNumberedLine(line: String): Array[Int] =
      line.map(char => if (char == 'o') 1 else 0).toArray
    this.board.map(line => toNumberedLine(line)).toArray
  }

  def isEdgeCase(): Boolean =
    (this.board.length == 1 || this.board(0).length == 1)

  def toSolvedMap(bsq: Square): Array[String] = {
    def addSolvedLine(index: Int, squareSize: Int, line: String): String = {
      val startOfLine = line.slice(0, index)
      val endLine =
        if (squareSize + index < line.length)
          line.slice(index + squareSize, line.length)
        else ""

      startOfLine + ("x" * squareSize) + endLine
    }

    def isLineWithSquare(bsq: Square, lineCount: Int): Boolean =
      bsq.x - lineCount >= 0 && bsq.x - lineCount < bsq.size

    for (idx <- this.board.indices)
      if (isLineWithSquare(bsq, idx))
        this.board(idx) =
          addSolvedLine(bsq.y - bsq.size + 1, bsq.size, this.board(idx))
    this.board
  }
}
