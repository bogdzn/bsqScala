package  bsq
import scala.annotation.tailrec

class Solver {
  def toIntMap(currentBoard: Option[Array[String]]): Option[Array[Array[Int]]] = {
    def getIntArray(str: String, expectedLength: Int): Option[Array[Int]] = {
      val emptySpot = 0
      val filledSpot = 1

      if (str.length != expectedLength) None else {
        Some(str.map(char => if ((char == 'o') || (char == '\n')) emptySpot
        else if (char == '.') filledSpot
        else return None).toArray)
      }
    }

    if (currentBoard.isEmpty) None else {
      val length = currentBoard.get(0).length

      Some(currentBoard.get.map(line => getIntArray(line, length).getOrElse(return None)))
    }
  }

  def solve(currentBoard: Option[Array[String]]): Option[Array[String]] = {
    def findBiggestSquare(board: Option[Array[Array[Int]]]): Square = {
      def updateBoardValues(board: Array[Array[Int]], square: Square): Array[Array[Int]] = {
        board(square.x)(square.y) = square.size
        board
      }

      @tailrec
      def getBiggestSquare(board: Array[Array[Int]], posOpt: Option[Square], bsq: Square): Square = {
        if (posOpt.isEmpty) bsq else {
          val pos = posOpt.get

          if (pos.isSquare(board)) {
            val newBsq = Square(pos.x, pos.y, pos.getSquareSize(board))
            val newBoard = updateBoardValues(board, newBsq)

            if (newBsq.size > bsq.size) getBiggestSquare(newBoard, pos.toNext(board), newBsq)
            else getBiggestSquare(newBoard, pos.toNext(board), bsq)
          } else getBiggestSquare(board, pos.toNext(board), bsq)
        }
      }
      val emptySquare = Square(0, 0, 0)
      val startPosition = Square(1, 1, 0)

      if (board.isEmpty) emptySquare
      else getBiggestSquare(board.get, Some(startPosition), emptySquare)
    }

    def getSolvedMap(bsq: Square, oldBoard: Array[String]): Array[String] = {
      def addSolvedLine(idx: Int, sqSize: Int, line: String): String = {
        val startOfLine = line.slice(0, idx)
        val endOfLine = if (sqSize + idx < line.length) line.slice(idx + sqSize, line.length) else ""
        startOfLine + ("x" * sqSize) + endOfLine
      }

      def isLineWithSquare(bsq: Square, lineCount: Int): Boolean = bsq.x - lineCount >= 0 && bsq.x - lineCount < bsq.size

      for (idx <- oldBoard.indices)
        if (isLineWithSquare(bsq, idx))
          oldBoard(idx) = addSolvedLine(bsq.y - bsq.size + 1, bsq.size, oldBoard(idx))
      oldBoard
    }

    def isEdgeCase(board: Array[String]): Boolean = (board.length == 1 || board(0).length == 1)

    def findFirstDot(board: Option[Array[Array[Int]]]): Square = {
      @tailrec
      def getFirstDot(board: Array[Array[Int]], pos: Square): Square = {
        def shouldGetToNextColumn(board: Array[Array[Int]], pos: Square): Boolean =
          board.length == 1 && !pos.isOutOfBounds(board, Square(pos.x, pos.y + 1, 1))

        def shouldGetToNextLine(board: Array[Array[Int]], pos: Square): Boolean =
          board(0).length == 1 && !pos.isOutOfBounds(board, Square(pos.x + 1, pos.y, 1))

        val filledSpot = 1
        val noSquareFound = Square(0, 0, 0)

        if (board(pos.x)(pos.y) == filledSpot) pos
        else if (shouldGetToNextColumn(board, pos)) getFirstDot(board, Square(pos.x, pos.y + 1, 1))
        else if (shouldGetToNextLine(board, pos))   getFirstDot(board, Square(pos.x + 1, pos.y, 1))
        else noSquareFound
      }

      if (board.isEmpty) Square(0, 0, 0) else getFirstDot(board.get, Square(0, 0, 1))
    }

    if (currentBoard.isEmpty) None else {
      val bsq: Square =
        if (isEdgeCase(currentBoard.get)) findFirstDot(toIntMap(currentBoard))
        else findBiggestSquare(toIntMap(currentBoard))

      if (bsq.size == 0) currentBoard else Some(getSolvedMap(bsq, currentBoard.get))
    }
  }
}
