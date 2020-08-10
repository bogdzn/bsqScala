import bsq._
import org.scalatest.funsuite.AnyFunSuite

class SetSpec extends AnyFunSuite {

  def generateIntArray(size: Int): Array[Array[Int]] = {
    var result: Array[Array[Int]] =
      for (idx <- 0 until size) result :+= Array.fill(size)(0 | 1)
    result
  }

  val posOutOfBounds: Square = Square(232, 12, 0)
  val posCorrect: Square = Square(1, 1, 0)
  val intBoard: Array[Array[Int]] = generateIntArray(10)


  test("isOutOfBounds") {
    val expectTrue = posOutOfBounds.isOutOfBounds(intBoard, posOutOfBounds)
    val expectFalse = posCorrect.isOutOfBounds(intBoard, posCorrect)

    assert(expectTrue, "Should be Out of Bounds")
    assert(!expectFalse, "Should not be Out of Bounds")
  }

  test("ToNext") {
    val expectFalse : Option[Square] = posOutOfBounds.toNext(intBoard)
    val expectTrue : Option[Square] = posCorrect.toNext(intBoard)

    assert(expectTrue.nonEmpty, "toNext should be ok for position(1, 1).")
    assert(expectTrue.get.x == 2, "expectTrue.x should be equal to 2.")
    assert(expectTrue.get.y == 1, "expectTrue.y should be equal to 1.")
    assert(expectFalse.isEmpty, "toNext should fail for position(232, 12)")
  }
}
