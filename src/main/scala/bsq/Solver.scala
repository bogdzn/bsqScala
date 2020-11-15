package bsq
import scala.annotation.tailrec

case class Solver(boardOpt: Option[Array[String]]) {
  type numBoard = Array[Array[Int]]

  private def toNextLine(brd: numBoard, pos: Square): Boolean =
    brd(0).length == 1 && !pos.isOutOfBounds(brd, Square(pos.x + 1, pos.y, 1))

  private def toNextColumn(board: numBoard, pos: Square): Boolean =
    board.length == 1 && !pos.isOutOfBounds(board, Square(pos.x, pos.y + 1, 1))

  @tailrec
  final def getFirstDot(board: numBoard, pos: Square): Square = {
    if (board(pos.x)(pos.y) == 1)
      pos
    else if (toNextColumn(board, pos))
      getFirstDot(board, Square(pos.x, pos.y + 1, 1))
    else if (toNextLine(board, pos))
      getFirstDot(board, Square(pos.x + 1, pos.y, 1))
    else Square(0, 0)
  }

  @tailrec
  final def getBiggestSquare(
      board: numBoard,
      posOpt: Option[Square],
      biggestSquare: Square
  ): Square = {
    if (posOpt.isEmpty)
      biggestSquare
    else if (posOpt.get.isSquare(board)) {
      val pos = posOpt.get
      val newBiggestSquare = Square(pos.x, pos.y, pos.getSquareSize(board))

      board(newBiggestSquare.x)(newBiggestSquare.y) = newBiggestSquare.size
      if (newBiggestSquare.size > biggestSquare.size)
        getBiggestSquare(board, pos.toNext(board), newBiggestSquare)
      else
        getBiggestSquare(board, pos.toNext(board), newBiggestSquare)
    } else getBiggestSquare(board, posOpt.get.toNext(board), biggestSquare)
  }

}
