package bsq

object bsq {
  def exitWith(value: Int, str: String): Unit = {
    System.err.println(str)
    sys.exit(value)
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 1)
      exitWith(1, "Error: Expected 1 argument.")

    val solvedBoard: solvedBoard = new solvedBoard(new FileContent(args.headOption))
    solvedBoard.result.foreach(e => println(e))
  }
}