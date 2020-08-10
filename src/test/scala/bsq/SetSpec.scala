import bsq._
import org.scalatest.funsuite.AnyFunSuite

class SetSpec extends AnyFunSuite {
  val validFile = new FileContent(Option("./maps/intermediate_34_137_empty"))
  val invalidFile = new FileContent(Option("./invalidFile"))

  val posOutOfBounds: Square = Square(232, 12, 0)
  val posCorrect: Square = Square(1, 1, 0)

  val board = new SolvedBoard(validFile)
  val intBoard: Array[Array[Int]] = board.toIntMap(validFile.content.get)



  test("isOutOfBounds") {
    val expectTrue = posOutOfBounds.isOutOfBounds(intBoard, posOutOfBounds)
    val expectFalse = posCorrect.isOutOfBounds(intBoard, posCorrect)

    assert(expectTrue, "Should be Out of Bounds")
    assert(!expectFalse, "Should not be Out of Bounds")
  }

  test("ToNext") {
    val expectFalse : Option[Square] = posOutOfBounds.toNext(intBoard)
    val expectTrue : Option[Square] = posCorrect.toNext(intBoard)

    assert(expectTrue.isDefined, "toNext should be ok for position(1, 1).")
    assert(expectTrue.get.x == 2, "expectTrue.x should be equal to 2.")
    assert(expectTrue.get.y == 1, "expectTrue.y should be equal to 1.")
    assert(expectFalse.isEmpty, "toNext should fail for position(232, 12)")
  }
}