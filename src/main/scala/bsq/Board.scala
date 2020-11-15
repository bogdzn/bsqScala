package bsq

case class Board (board: Array[String]) {

  def toIntBoard(): Array[Array[Int]] = ???

  def isEdgeCase(): Boolean =
    (this.board.length == 1 || this.board(0).length == 1 )

  def toSolvedMap(board: Array[String], biggestSquare: Square): Array[String] = ???


}
