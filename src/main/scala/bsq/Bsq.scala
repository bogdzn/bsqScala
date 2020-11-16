package bsq

object Bsq {
  def main(args: Array[String]): Unit = {
    val file = new FileHandler
    val fileContent = file.read(args.headOption)
    val solver = new Solver(fileContent)

    if (fileContent.isEmpty || fileContent.get.isEmpty) {
      println("Error reading file.")
    } else {
      val board = Board(fileContent.get)
      val numBoard = board.toIntBoard()
      val biggestSquare =
        if (board.isEdgeCase())
          solver.getFirstDot(numBoard, Square(0, 0))
        else solver.getBiggestSquare(numBoard, Some(Square(1, 1)), Square(0, 0))
      val solvedBoard = board.toSolvedBoard(biggestSquare)

      solvedBoard.foreach(line => println(line))
    }
  }
}
