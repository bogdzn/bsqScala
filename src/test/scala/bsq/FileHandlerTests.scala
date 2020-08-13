import bsq._
import org.scalatest.funsuite.AnyFunSuite

class FileHandlerTests extends AnyFunSuite {
    val validFilename: Option[String] = Option("./maps/intermediate_map_100_100")
    val invalidFilename: Option[String] = Option("./maps/fortnite")
    val emptyFilename: Option[String] = Option("")
    val noFilename: Option[String] = None

    val fc = new FileHandler

    test("invalidFilename.") {
        val board = fc.getFile(invalidFilename)

        assert(board.isEmpty, "getFile should fail.")
    }

    test("noFilename.") {
        val board = fc.getFile(noFilename)

        assert(board.isEmpty, "getFile should fail.")
    }

    test("validFilename.") {
        val board = fc.getFile(validFilename)

        assert(board.nonEmpty, "getFile should not return None.")
        assert(board.get.length == 100, "getFile should be 100 lines long.")
        assert(board.get.head.length == 100, "getLines's first line should be 100 characters long.")
    }

    test("isEmpty.") {
        val board = fc.getFile(Option("build.sbt"))
        val bool = fc.isEmpty(board)

        assert(!bool, "Map should not be empty.")
    }
}