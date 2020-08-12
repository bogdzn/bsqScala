package bsq

object Bsq {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Error: Expected 1 argument.")
    } else {
      val solvedBoard: SolvedBoard = new SolvedBoard(new FileContent(args.headOption))
      if (solvedBoard.result.isEmpty) println("Error during map parsing.")
      else solvedBoard.result.get.foreach(e => println(e))
    }
  }
}