package  bsq
import scala.annotation.tailrec

class SolvedBoard(currentBoard: FileContent) {
  def exitWith(code: Int, str: String): Int = {
    System.err.println(str)
    sys.exit(code)
  }

  def solve(currentBoard: Array[String]): Array[String] = {
    case class Square(x: Int, y: Int, size: Int) {
      def isOutOfBounds(optBoard: Array[Array[Int]], pos: Square): Boolean = {
        ((pos.x < 0 || pos.y < 0) || (pos.x > optBoard.length - 1 || pos.y > optBoard(pos.x).length - 1))
      }

      def toNext(board: Array[Array[Int]]): Option[Square] = {
        if (!isOutOfBounds(board, Square(this.x, this.y + 1, 0))) Some(Square(this.x, this.y + 1, 0))
        else if (!isOutOfBounds(board, Square(this.x + 1, 1, 0))) Some(Square(this.x + 1, 1, 0))
        else None
      }

      def groupDots(dots: Array[Array[Int]]): Array[Int] =
        Array(dots(this.x - 1)(this.y - 1), dots(this.x)(this.y - 1), dots(this.x - 1)(this.y))

      def isSquare(board: Array[Array[Int]]): Boolean = (groupDots(board).min > 0 && board(this.x)(this.y) > 0)

      def getSquareSize(board: Array[Array[Int]]): Int = groupDots(board).min + 1
    }

    def toIntMap(currentBoard: Array[String]): Array[Array[Int]] = {
      def getIntArray(str: String, expectedLength: Int): Array[Int] = {
        if (str.length != expectedLength)
          exitWith(3, "Error: Every line should have the same length.")
        str.map(char => if ((char == 'o') || (char == '\n')) 0
        else if (char == '.') 1
        else exitWith(2, s"Error: Unknown $char character.")).toArray
      }

      val length = currentBoard(0).length
      currentBoard.map(line => getIntArray(line, length))
    }

    def findBiggestSquare(board: Array[Array[Int]]): Square = {
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

      getBiggestSquare(board, Some(Square(1, 1, 0)), Square(0, 0, 0))
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

    def findFirstDot(board: Array[Array[Int]]): Square = {
      @tailrec
      def getFirstDot(board: Array[Array[Int]], pos: Square): Square = {
        if (board(pos.x)(pos.y) == '.') pos
        else if (board.length == 1 && !pos.isOutOfBounds(board, Square(pos.x, pos.y + 1, 1)))
          getFirstDot(board, Square(pos.x, pos.y + 1, 1))
        else if (board(0).length == 1 && !pos.isOutOfBounds(board, Square(pos.x + 1, pos.y, 1)))
          getFirstDot(board, Square(pos.x + 1, pos.y, 1))
        else Square(0, 0, 0)
      }

      getFirstDot(board, Square(0, 0, 1))
    }

    val bsq: Square = if (isEdgeCase(currentBoard)) findFirstDot(toIntMap(currentBoard))
    else findBiggestSquare(toIntMap(currentBoard))
    if (bsq.size == 0) currentBoard else getSolvedMap(bsq, currentBoard)
  }

  if (currentBoard.content.isEmpty) exitWith(1, "Empty file content.")
  val result: Array[String] = solve(currentBoard.content.get)
}
