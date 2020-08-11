import bsq._
import org.scalatest.funsuite.AnyFunSuite

class FileContentTests extends AnyFunSuite {
    val validFilename = Option("./maps/intermediate_map_100_100")
    val invalidFilename = Option("./maps/fortnite")
    val emptyFilename = Option("")
    val noFilename = None

    val fc = new FileContent(validFilename)

    test("invalidFilename") {
        val board = fc.getFile(invalidFilename)

        assert(board.isEmpty, "getFile should fail.")
    }

    test("noFilename") {
        val board = fc.getFile(noFilename)

        assert(board.isEmpty, "getFile should fail.")
    }

    test("validFilename") {
        val board = fc.getFile(validFilename)

        assert(board.nonEmpty, "getFile should not return None.")
        assert(board.get.length == 100, "getFile should be 100 lines long.")
        assert(board.get.head.length == 100, "getLines's first line should be 100 characters long.")
    }
}