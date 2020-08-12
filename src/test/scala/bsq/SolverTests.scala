import bsq._
import org.scalatest.funsuite.AnyFunSuite

class SolvedBoardTests extends AnyFunSuite {
  val fileContent = new FileHandler
  val board = new Solver

  def genStringArr(size: Int, fillWith: String): Array[String] =
    Array.fill[String](size)(fillWith)

  test("toIntMap is valid") {
    val validBoard = Some(genStringArr(10, "....o.oo.o."))

    assert(board.toIntMap(validBoard).nonEmpty, "intBoard should not be empty.")
  }

  test("toIntMap is invalid") {
    val invalidBoard = Some(genStringArr(10, "..asdasd..."))

    assert(board.toIntMap(invalidBoard).isEmpty, "intBoard should be empty.")
  }

  test("toIntMap is empty") {
    val emptyBoard: Option[Array[String]] = None

    assert(board.toIntMap(emptyBoard).isEmpty, "intBoard should be empty.")
  }

  test("toIntMap is uneven") {
    val newBoard = Array("....o...o.o..", ".....", "..o.......")

    assert(board.toIntMap(Option(newBoard)).isEmpty, "Uneven boards should return None.")
  }


}