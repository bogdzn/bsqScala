import bsq._
import org.scalatest.funsuite.AnyFunSuite

class FileHandlerTests extends AnyFunSuite {
  val validFilename: Option[String] = Option("./maps/intermediate_map_100_100")
  val invalidFilename: Option[String] = Option("./maps/fortnite")
  val emptyFilename: Option[String] = Option("")
  val noFilename: Option[String] = None

  val fc = FileHandler()

  test("invalidFilename.") {
    val board = fc.read(invalidFilename)

    assert(board.isEmpty, "getFile should fail.")
  }

  test("noFilename.") {
    val board = fc.read(noFilename)

    assert(board.isEmpty, "getFile should fail.")
  }

  test("validFilename.") {
    val board = fc.read(validFilename)

    assert(board.nonEmpty, "getFile should not return None.")
    assert(board.get.length == 100, "getFile should be 100 lines long.")
    assert(
      board.get.head.length == 100,
      "getLines's first line should be 100 characters long."
    )
  }
}

