package bsq

case class Board(board: Array[String]) {

  private def isValidLine(expectedLength: Int, line: String): Boolean = {
    if (expectedLength != line.length())
      false
    else if (line.exists(char => char != 'o' && char != '.' && char != '\n'))
      false
    else true
  }

  private def toNumberedLine(line: String): Array[Int] =
    line.map(char => if (char == '.') 1 else 0).toArray

  private def updateLine(index: Int, squareSize: Int, line: String): String = {
    val startOfLine = line.slice(0, index)
    val endLine =
      if (squareSize + index < line.length)
        line.slice(index + squareSize, line.length)
      else ""

    startOfLine + ("x" * squareSize) + endLine
  }

  private def isLineWithSquare(sq: Square, lineCount: Int): Boolean =
    sq.x - lineCount >= 0 && sq.x - lineCount < sq.size

    def isValidBoard(): Boolean = {
    val boardSize = this.board.size
    val expectedLen = if (boardSize == 0) 0 else this.board(0).length()

    this.board.filter(str => isValidLine(expectedLen, str)).length == boardSize
  }

  def toIntBoard(): Array[Array[Int]] =
    this.board.map(line => toNumberedLine(line)).toArray

  def isEdgeCase(): Boolean =
    (this.board.length == 1 || this.board(0).length == 1)

  def toSolvedMap(s: Square): Array[String] = {
    for (idx <- this.board.indices) {
      if (isLineWithSquare(s, idx))
        this.board(idx) = updateLine(s.y - s.size + 1, s.size, this.board(idx))
    }

    this.board
  }
}
