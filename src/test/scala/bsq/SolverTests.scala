import bsq._
import org.scalatest.funsuite.AnyFunSuite

class SolvedBoardTests extends AnyFunSuite {
  val fileContent = FileHandler()

  def genStringArr(size: Int, fillWith: String): Array[String] =
    Array.fill[String](size)(fillWith)

  def compareSolvedMap(path: String): Boolean = {
    def wrapperSolver(filepath: String): Array[String] = {
      val fc = fileContent.read(Some(filepath))

      if (fc.isEmpty) {
        assert(fc.nonEmpty, s"Error: $filepath should not be empty.")
        Array[String]()
      } else {
        val board = Board(fc.get)
        val numBoard = board.toIntBoard()
        val solver = Solver(fc)
        val bsq =
          if (board.isEdgeCase()) solver.getFirstDot(numBoard, Square(0, 0))
          else
            solver.getBiggestSquare(numBoard, Some(Square(1, 1)), Square(0, 0))

        val brd = board.toSolvedBoard(bsq)

        assert(brd.nonEmpty, s"$filepath should not be empty after solver.")
        brd
      }
    }

    def compareTwoArrays(
        boardOne: Array[String],
        boardTwo: Array[String]
    ): Boolean = {
      assert(
        boardOne.length == boardTwo.length,
        s"Solved board should be ${boardTwo.length} lines long."
      )
      assert(
        boardOne(0).length == boardTwo(0).length,
        s"Solved board's lines should be ${boardTwo(0).length} characters long."
      )

      for (idx <- boardOne.indices)
        if (boardOne(idx) != boardTwo(idx)) return false
      true
    }

    val expectedResult = wrapperSolver(s"./solved_maps/$path")
    val solvedBoard = wrapperSolver(s"./maps/$path")

    compareTwoArrays(solvedBoard, expectedResult)
  }

  def groupTests(specifier: String): Unit = {
    assert(
      compareSolvedMap(s"intermediate_map_${specifier}_empty"),
      s"${specifier} empty failed."
    )
    assert(
      compareSolvedMap(s"intermediate_map_${specifier}_filled"),
      s"${specifier} filled failed."
    )
    assert(
      compareSolvedMap(s"intermediate_map_${specifier}_with_obstacles_25pc"),
      s"${specifier} 25pc failed."
    )
    assert(
      compareSolvedMap(s"intermediate_map_${specifier}_with_obstacles_50pc"),
      s"${specifier} 50pc failed."
    )
    assert(
      compareSolvedMap(s"intermediate_map_${specifier}_with_obstacles_75pc"),
      s"${specifier} 75pc failed."
    )
  }

  test("filled board 34x137") {
    compareSolvedMap("./maps/intermediate_map_34_137_filled")
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
    assert(
      compareSolvedMap("intermediate_map_1000_1000"),
      "1000x1000(2) failed."
    )
  }

  test("testing edge cases...") {
    assert(
      compareSolvedMap("intermediate_map_empty_corners"),
      "empty corners failed."
    )
    assert(
      compareSolvedMap("intermediate_map_filled_corners"),
      "filled corners failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_empty_column"),
      "One-column empty map failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_filled_column"),
      "One-column filled map failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_column_with_obstacles_25pc"),
      "One-column 25pc failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_column_with_obstacles_50pc"),
      "One-column 50pc failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_column_with_obstacles_75pc"),
      "One-column 75pc failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_empty_line"),
      "One-line empty map failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_filled_line"),
      "One-line filled map failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_line_with_obstacles_25pc"),
      "One-line 25pc failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_line_with_obstacles_50pc"),
      "One-line 50pc failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_line_with_obstacles_75pc"),
      "One-line 75pc failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_empty_box"),
      "empty box failed."
    )
    assert(
      compareSolvedMap("intermediate_map_one_filled_box"),
      "filled box failed."
    )
  }
}
