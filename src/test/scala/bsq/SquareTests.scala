import bsq._
import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random

class SquareTests extends AnyFunSuite {
  def generateIntArray(size: Int): Array[Array[Int]] = {
    def createSingleArray(size: Int): Array[Int] = {
      val rand = new Random()
      Array.fill[Int](size)(0 + rand.nextInt(1))
    }
    Array.fill[Array[Int]](size)(createSingleArray(size))
  }

  def modifyBoard(
      targetPos: Square,
      modification: Int,
      board: Array[Array[Int]]
  ): Array[Array[Int]] = {
    board(targetPos.x)(targetPos.y) = modification
    board(targetPos.x - 1)(targetPos.y) = modification
    board(targetPos.x - 1)(targetPos.y - 1) = modification
    board(targetPos.x)(targetPos.y - 1) = modification
    board
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
    val posAtEndOfLine = Square(1, 9, 0)

    val expectFalse: Option[Square] = posOutOfBounds.toNext(intBoard)
    val expectTrue: Option[Square] = posCorrect.toNext(intBoard)
    val expectTrueAlso: Option[Square] = posAtEndOfLine.toNext(intBoard)

    assert(expectTrue.nonEmpty, "toNext should be ok for position(1, 1).")
    assert(expectTrue.get.x == 1, "expectTrue.x should be equal to 1.")
    assert(expectTrue.get.y == 2, "expectTrue.y should be equal to 2.")
    assert(expectFalse.isEmpty, "toNext should fail for position(232, 12)")
    assert(expectTrueAlso.nonEmpty, "toNext should be ok for position(1, 9).")
    assert(expectTrueAlso.get.x == 2, "Position realignement error.")
    assert(expectTrueAlso.get.y == 1, "Position realignement error.")
  }

  test("isSquare") {
    val expectTrue = posCorrect.isSquare(modifyBoard(posCorrect, 1, intBoard))
    val expectFalse = posCorrect.isSquare(modifyBoard(posCorrect, 0, intBoard))

    assert(expectTrue, "Position(1, 1) should find a Square.")
    assert(!expectFalse, "Position(1, 1) should not find a Square.")
  }

  test("getSquareSize") {
    val size = posCorrect.getSquareSize(modifyBoard(posCorrect, 1, intBoard))

    assert(size == 2, "Size should be equal to Two.")
  }
}
