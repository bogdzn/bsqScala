package  bsq
import scala.annotation.tailrec

class SolvedBoard(currentBoard: FileContent) {
  def toIntMap(currentBoard: Option[Array[String]]): Option[Array[Array[Int]]] = {
    def getIntArray(str: String, expectedLength: Int): Option[Array[Int]] = {
      if (str.length != expectedLength) None
      else {
        Some(str.map(char => if ((char == 'o') || (char == '\n')) 0
        else if (char == '.') 1
        else return None).toArray)
      }
    }

    if (currentBoard.isEmpty) None
    val length = currentBoard.get(0).length
    Some(currentBoard.get.map(line =>
      if (getIntArray(line, length).isEmpty) return None
      else getIntArray(line, length).get)
    )
  }

  def solve(currentBoard: Option[Array[String]]): Option[Array[String]] = {
    def findBiggestSquare(board: Option[Array[Array[Int]]]): Square = {
      def updateBoardValues(board: Array[Array[Int]], square: Square): Array[Array[Int]] = {
        board(square.x)(square.y) = square.size
        board
      }

      @tailrec
      def getBiggestSquare(board: Array[Array[Int]], posOpt: Option[Square], bsq: Square): Square = {
        if (posOpt.isEmpty) bsq
        else {
          val pos = posOpt.getOrElse(Square(1, 1, 0))
          if (pos.isSquare(board)) {
            val newBsq = Square(pos.x, pos.y, pos.getSquareSize(board))
            if (newBsq.size > bsq.size)
              getBiggestSquare(updateBoardValues(board, newBsq), pos.toNext(board), newBsq)
            else getBiggestSquare(updateBoardValues(board, newBsq), pos.toNext(board), bsq)
          } else getBiggestSquare(board, pos.toNext(board), bsq)
        }
      }

      if (board.isEmpty) Square(0, 0, 0)
      else getBiggestSquare(board.get, Some(Square(1, 1, 0)), Square(0, 0, 0))
    }

    def getSolvedMap(bsq: Square, oldBoard: Array[String]): Array[String] = {
      def addSolvedLine(idx: Int, sqSize: Int, line: String): String = {
        val startOfLine = line.slice(0, idx)
        val endOfLine = if (sqSize + idx < line.length) line.slice(idx + sqSize, line.length) else ""
        startOfLine + ("x" * sqSize) + endOfLine
      }

      def isLineWithSquare(bsq: Square, lineCount: Int): Boolean = (bsq.x - lineCount >= 0 && bsq.x - lineCount <= bsq.size)

      for (idx <- oldBoard.indices)
        if (isLineWithSquare(bsq, idx))
          oldBoard(idx) = addSolvedLine(bsq.y - bsq.size + 1, bsq.size, oldBoard(idx))
      oldBoard
    }

    def isEdgeCase(board: Array[String]): Boolean = (board.length == 1 || board(0).length == 1)

    def findFirstDot(board: Option[Array[Array[Int]]]): Square = {
      @tailrec
      def getFirstDot(board: Array[Array[Int]], pos: Square): Square = {
        if (board(pos.x)(pos.y) == '.') pos
        else if (board.length == 1 && !pos.isOutOfBounds(board, Square(pos.x, pos.y + 1, 1)))
          getFirstDot(board, Square(pos.x, pos.y + 1, 1))
        else if (board(0).length == 1 && !pos.isOutOfBounds(board, Square(pos.x + 1, pos.y, 1)))
          getFirstDot(board, Square(pos.x + 1, pos.y, 1))
        else Square(0, 0, 0)
      }

      if (board.isEmpty) Square(0, 0, 0)
      else getFirstDot(board.get, Square(0, 0, 1))
    }

    if (currentBoard.isEmpty) {
      println("Error: Board could not be read.")
      None
    } else {
      val bsq: Square = if (isEdgeCase(currentBoard.get)) findFirstDot(toIntMap(currentBoard))
      else findBiggestSquare(toIntMap(currentBoard))

      if (bsq.size == 0) currentBoard
      else Some(getSolvedMap(bsq, currentBoard.get))
    }
  }

  val result: Option[Array[String]] = solve(currentBoard.content)
}
