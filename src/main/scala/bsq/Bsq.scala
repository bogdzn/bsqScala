package bsq

object Bsq {
  def main(args: Array[String]): Unit = {
    val file = new FileHandler
    val solver = Solver(file.read(args.headOption))

    solver.solve().map(_.foreach(println(_)))
  }
}
