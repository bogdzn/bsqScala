package bsq

case class Square(x: Int, y: Int, size: Int = 0) {
  def isOutOfBounds(optBoard: Array[Array[Int]], pos: Square): Boolean =
    (pos.x < 0 || pos.y < 0) || (pos.x > optBoard.length - 1 || pos.y > optBoard(
      pos.x
    ).length - 1)

  def toNext(board: Array[Array[Int]]): Option[Square] = {
    if (!isOutOfBounds(board, Square(this.x, this.y + 1)))
      Some(Square(this.x, this.y + 1))
    else if (!isOutOfBounds(board, Square(this.x + 1, 1)))
      Some(Square(this.x + 1, 1))
    else None
  }

  private def groupDots(dots: Array[Array[Int]]): Array[Int] =
    Array(
      dots(this.x - 1)(this.y - 1),
      dots(this.x)(this.y - 1),
      dots(this.x - 1)(this.y)
    )

  def isSquare(board: Array[Array[Int]]): Boolean =
    groupDots(board).min > 0 && board(this.x)(this.y) > 0

  def getSquareSize(board: Array[Array[Int]]): Int = groupDots(board).min + 1
}
