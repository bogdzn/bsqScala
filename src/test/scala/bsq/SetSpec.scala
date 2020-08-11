import bsq._
import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random

class SetSpec extends AnyFunSuite {

  def generateIntArray(size: Int): Array[Array[Int]] = {
    def createSingleArray(size: Int): Array[Int] = {
      val rand = new Random()
      Array.fill[Int](size)(0 + rand.nextInt(1))
    }
    Array.fill[Array[Int]](size)(createSingleArray(size))
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

  test("toNext") {
    val expectFalse : Option[Square] = posOutOfBounds.toNext(intBoard)
    val expectTrue : Option[Square] = posCorrect.toNext(intBoard)

    assert(expectTrue.nonEmpty, "toNext should be ok for position(1, 1).")
    assert(expectTrue.get.x == 1, "expectTrue.x should be equal to 1.")
    assert(expectTrue.get.y == 2, "expectTrue.y should be equal to 2.")
    assert(expectFalse.isEmpty, "toNext should fail for position(232, 12)")
  }
}
