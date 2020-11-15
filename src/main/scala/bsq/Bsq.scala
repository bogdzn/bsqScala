package bsq

object Bsq {
  def main(args: Array[String]): Unit = {
    if (args.length != 1) {
      println("Error: Expected 1 argument.")
    } else {
      val solver = new Solver
      val file = new FileHandler
      val board = solver.solve(file.getFile(args.headOption))

      if (board.isEmpty) println("Error during map parsing.")
      else board.get.foreach(e => println(e))
    }
  }
}

