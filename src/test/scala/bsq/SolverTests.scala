import bsq._
import org.scalatest.funsuite.AnyFunSuite

class SolvedBoardTests extends AnyFunSuite {
  val fileContent = new FileHandler
  val board = new Solver

  def genStringArr(size: Int, fillWith: String): Array[String] =
    Array.fill[String](size)(fillWith)

  def compareSolvedMap(path: String): Boolean = {
    def wrapperSolver(filepath: String): Array[String] = {
      val fc = fileContent.getFile(Option(filepath))
      val brd = board.solve(fc)

      assert(brd.nonEmpty, s"$filepath should not be empty after solver.")
      brd.get
    }

    def compareTwoArrays(boardOne: Array[String], boardTwo: Array[String]): Boolean = {
      assert(boardOne.length == boardTwo.length, s"Solved board should be ${boardTwo.length} lines long.")
      assert(boardOne(0).length == boardTwo(0).length, s"Solved board's lines should be ${boardTwo(0).length} characters long.")

      for (idx <- boardOne.indices)
        if (boardOne(idx) != boardTwo(idx)) return false
      true
    }

    val expectedResult = wrapperSolver(s"./solved_maps/$path")
    val solvedBoard = wrapperSolver(s"./maps/$path")

    compareTwoArrays(solvedBoard, expectedResult)
  }

  def groupTests(specifier: String): Unit = {
    assert(compareSolvedMap(s"intermediate_map_${specifier}_empty"), s"${specifier} empty failed.")
    assert(compareSolvedMap(s"intermediate_map_${specifier}_filled"), s"${specifier} filled failed.")
    assert(compareSolvedMap(s"intermediate_map_${specifier}_with_obstacles_25pc"), s"${specifier} 25pc failed.")
    assert(compareSolvedMap(s"intermediate_map_${specifier}_with_obstacles_50pc"), s"${specifier} 50pc failed.")
    assert(compareSolvedMap(s"intermediate_map_${specifier}_with_obstacles_75pc"), s"${specifier} 75pc failed.")
  }

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

  test("filled board 34x137") {
    val testBoard = fileContent.getFile(Option("./maps/intermediate_map_34_137_filled"))
    val resultBoard = board.solve(testBoard)

    assert(resultBoard.nonEmpty, "result board should not be empty.")

    val confirmedBoard = resultBoard.get

    assert(confirmedBoard.length == 137, "result should be 34 lines long.")
    assert(confirmedBoard(0).length == 34, "result's lines should be 127 characters long.")
  }

  test("small maps....") {
    groupTests("34_137")
    groupTests("97_21")
    assert(compareSolvedMap("intermediate_map_100_100"), "100x100 failed.")
    groupTests("187_187")
    assert(compareSolvedMap("intermediate_map_200_200"), "200x200 failed.")
  }

  test("middle-size maps...") {
    assert(compareSolvedMap("intermediate_map_500_500"), "500x500 failed.")
    assert(compareSolvedMap("intermediate_map_500_500_2"), "500x500(2) failed.")
    assert(compareSolvedMap("intermediate_map_500_500_3"), "500x500(3) failed.")
    assert(compareSolvedMap("intermediate_map_1000_1000"), "1000x1000 failed.")
    assert(compareSolvedMap("intermediate_map_1000_1000"), "1000x1000(2) failed.")
  }

  test("testing edge cases...") {
    assert(compareSolvedMap("intermediate_map_empty_corners"), "empty corners failed.")
    assert(compareSolvedMap("intermediate_map_filled_corners"), "filled corners failed.")
    assert(compareSolvedMap("intermediate_map_one_empty_column"), "One-column empty map failed.")
    assert(compareSolvedMap("intermediate_map_one_filled_column"), "One-column filled map failed.")
    assert(compareSolvedMap("intermediate_map_one_column_with_obstacles_25pc"), "One-column 25pc failed.")
    assert(compareSolvedMap("intermediate_map_one_column_with_obstacles_50pc"), "One-column 50pc failed.")
    assert(compareSolvedMap("intermediate_map_one_column_with_obstacles_75pc"), "One-column 75pc failed.")
    assert(compareSolvedMap("intermediate_map_one_empty_line"), "One-line empty map failed.")
    assert(compareSolvedMap("intermediate_map_one_filled_line"), "One-line filled map failed.")
    assert(compareSolvedMap("intermediate_map_one_line_with_obstacles_25pc"), "One-line 25pc failed.")
    assert(compareSolvedMap("intermediate_map_one_line_with_obstacles_50pc"), "One-line 50pc failed.")
    assert(compareSolvedMap("intermediate_map_one_line_with_obstacles_75pc"), "One-line 75pc failed.")
    assert(compareSolvedMap("intermediate_map_one_empty_box"), "empty box failed.")
    assert(compareSolvedMap("intermediate_map_one_filled_box"), "filled box failed.")
  }
}