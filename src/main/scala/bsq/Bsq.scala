package bsq

object Bsq {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) ExitWith(1, "Error: Expected 1 argument.")

    val solvedBoard: SolvedBoard = new SolvedBoard(new FileContent(args.headOption))
    solvedBoard.result.foreach(e => println(e))
  }
}