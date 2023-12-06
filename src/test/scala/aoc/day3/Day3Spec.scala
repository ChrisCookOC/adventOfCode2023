package aoc.day3

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day3Spec extends AnyWordSpec with Matchers {

  private val day3 = Day3()

  private val grid: List[List[Cell]] =
    List(
      List(
        Cell("4", Coord(0, 0)),
        Cell("6", Coord(0, 1)),
        Cell("7", Coord(0, 2)),
        Cell(".", Coord(0, 3)),
        Cell(".", Coord(0, 4)),
        Cell("1", Coord(0, 5)),
        Cell("1", Coord(0, 6)),
        Cell("4", Coord(0, 7)),
        Cell(".", Coord(0, 8)),
        Cell(".", Coord(0, 9))
      ),
      List(
        Cell(".", Coord(1, 0)),
        Cell(".", Coord(1, 1)),
        Cell(".", Coord(1, 2)),
        Cell("*", Coord(1, 3)),
        Cell(".", Coord(1, 4)),
        Cell(".", Coord(1, 5)),
        Cell(".", Coord(1, 6)),
        Cell(".", Coord(1, 7)),
        Cell(".", Coord(1, 8)),
        Cell(".", Coord(1, 9))
      ),
      List(
        Cell(".", Coord(2, 0)),
        Cell(".", Coord(2, 1)),
        Cell("3", Coord(2, 2)),
        Cell("5", Coord(2, 3)),
        Cell(".", Coord(2, 4)),
        Cell(".", Coord(2, 5)),
        Cell("6", Coord(2, 6)),
        Cell("3", Coord(2, 7)),
        Cell("3", Coord(2, 8)),
        Cell(".", Coord(2, 9))
      )
    )

  private val gridForTestingGearNumbers: List[List[Cell]] =
    List(
      List(
        Cell("4", Coord(0, 0)),
        Cell("6", Coord(0, 1)),
        Cell("7", Coord(0, 2)),
        Cell(".", Coord(0, 3)),
        Cell(".", Coord(0, 4)),
        Cell("1", Coord(0, 5)),
        Cell("1", Coord(0, 6)),
        Cell("4", Coord(0, 7)),
        Cell(".", Coord(0, 8)),
        Cell(".", Coord(0, 9))
      ),
      List(
        Cell("*", Coord(1, 0)),
        Cell(".", Coord(1, 1)),
        Cell(".", Coord(1, 2)),
        Cell("*", Coord(1, 3)),
        Cell(".", Coord(1, 4)),
        Cell(".", Coord(1, 5)),
        Cell(".", Coord(1, 6)),
        Cell(".", Coord(1, 7)),
        Cell(".", Coord(1, 8)),
        Cell("*", Coord(1, 9))
      ),
      List(
        Cell("*", Coord(2, 0)),
        Cell(".", Coord(2, 1)),
        Cell("3", Coord(2, 2)),
        Cell("5", Coord(2, 3)),
        Cell(".", Coord(2, 4)),
        Cell(".", Coord(2, 5)),
        Cell("6", Coord(2, 6)),
        Cell("3", Coord(2, 7)),
        Cell("3", Coord(2, 8)),
        Cell(".", Coord(2, 9))
      )
    )

  "parseLine" should {

    "parse line into array" in {

      day3.parseLine(("467..114..", 1)) shouldBe List(
        Cell("4", Coord(1, 0)),
        Cell("6", Coord(1, 1)),
        Cell("7", Coord(1, 2)),
        Cell(".", Coord(1, 3)),
        Cell(".", Coord(1, 4)),
        Cell("1", Coord(1, 5)),
        Cell("1", Coord(1, 6)),
        Cell("4", Coord(1, 7)),
        Cell(".", Coord(1, 8)),
        Cell(".", Coord(1, 9))
      )
      day3.parseLine(("617*......", 2)) shouldBe List(
        Cell("6", Coord(2, 0)),
        Cell("1", Coord(2, 1)),
        Cell("7", Coord(2, 2)),
        Cell("*", Coord(2, 3)),
        Cell(".", Coord(2, 4)),
        Cell(".", Coord(2, 5)),
        Cell(".", Coord(2, 6)),
        Cell(".", Coord(2, 7)),
        Cell(".", Coord(2, 8)),
        Cell(".", Coord(2, 9))
      )


    }

  }

  "parseGrid" should {
    "parse the grid into a 2D array" in {

      val table = "467..114..\n...*......\n..35..633."

      day3.parseTable(table) shouldBe grid

    }

    "parse the grid when numbers at end of line" in {
      val table = "467..1..14\n...*......\n..35...633"

      val expectedGrid: List[List[Cell]] =
        List(
          List(
            Cell("4", Coord(0, 0)),
            Cell("6", Coord(0, 1)),
            Cell("7", Coord(0, 2)),
            Cell(".", Coord(0, 3)),
            Cell(".", Coord(0, 4)),
            Cell("1", Coord(0, 5)),
            Cell(".", Coord(0, 6)),
            Cell(".", Coord(0, 7)),
            Cell("1", Coord(0, 8)),
            Cell("4", Coord(0, 9))
          ),
          List(
            Cell(".", Coord(1, 0)),
            Cell(".", Coord(1, 1)),
            Cell(".", Coord(1, 2)),
            Cell("*", Coord(1, 3)),
            Cell(".", Coord(1, 4)),
            Cell(".", Coord(1, 5)),
            Cell(".", Coord(1, 6)),
            Cell(".", Coord(1, 7)),
            Cell(".", Coord(1, 8)),
            Cell(".", Coord(1, 9))
          ),
          List(
            Cell(".", Coord(2, 0)),
            Cell(".", Coord(2, 1)),
            Cell("3", Coord(2, 2)),
            Cell("5", Coord(2, 3)),
            Cell(".", Coord(2, 4)),
            Cell(".", Coord(2, 5)),
            Cell(".", Coord(2, 6)),
            Cell("6", Coord(2, 7)),
            Cell("3", Coord(2, 8)),
            Cell("3", Coord(2, 9))
          )
        )

      day3.parseTable(table) shouldBe expectedGrid
    }

  }

  "getDiagonalCellsFromArray" should {

    "get diagonals from a given index" in {
      day3.getDiagonalCellsFromArray(grid, Coord(1, 1)) shouldBe List(
        Cell("4", Coord(0, 0)),
        Cell("6", Coord(0, 1)),
        Cell("7", Coord(0, 2)),
        Cell(".", Coord(1, 0)),
        Cell(".", Coord(1, 2)),
        Cell(".", Coord(2, 0)),
        Cell(".", Coord(2, 1)),
        Cell("3", Coord(2, 2))
      )

    }

    "not go negative" in {

      day3.getDiagonalCellsFromArray(grid, Coord(0, 1)) shouldBe List(
        Cell("4", Coord(0, 0)),
        Cell("7", Coord(0, 2)),
        Cell(".", Coord(1, 0)),
        Cell(".", Coord(1, 1)),
        Cell(".", Coord(1, 2)),
      )

      day3.getDiagonalCellsFromArray(grid, Coord(1, 0)) shouldBe List(
        Cell("4", Coord(0, 0)),
        Cell("6", Coord(0, 1)),
        Cell(".", Coord(1, 1)),
        Cell(".", Coord(2, 0)),
        Cell(".", Coord(2, 1)),
      )

    }

    "not go off the grid" in {

      day3.getDiagonalCellsFromArray(grid, Coord(2, 9)) shouldBe List(
        Cell(".", Coord(1, 8)),
        Cell(".", Coord(1, 9)),
        Cell("3", Coord(2, 8))
      )
      day3.getDiagonalCellsFromArray(grid, Coord(2, 1)) shouldBe List(
        Cell(".", Coord(1, 0)),
        Cell(".", Coord(1, 1)),
        Cell(".", Coord(1, 2)),
        Cell(".", Coord(2, 0)),
        Cell("3", Coord(2, 2))
      )
      day3.getDiagonalCellsFromArray(grid, Coord(1, 9)) shouldBe List(
        Cell(".", Coord(0, 8)),
        Cell(".", Coord(0, 9)),
        Cell(".", Coord(1, 8)),
        Cell("3", Coord(2, 8)),
        Cell(".", Coord(2, 9))
      )

    }

    "handle the corners" in {
      day3.getDiagonalCellsFromArray(grid, Coord(0, 0)) shouldBe List(
        Cell("6", Coord(0, 1)),
        Cell(".", Coord(1, 0)),
        Cell(".", Coord(1, 1)),
      )
      day3.getDiagonalCellsFromArray(grid, Coord(0, 9)) shouldBe List(
        Cell(".", Coord(0, 8)),
        Cell(".", Coord(1, 8)),
        Cell(".", Coord(1, 9)),
      )
      day3.getDiagonalCellsFromArray(grid, Coord(2, 9)) shouldBe List(
        Cell(".", Coord(1, 8)),
        Cell(".", Coord(1, 9)),
        Cell("3", Coord(2, 8)),
      )
      day3.getDiagonalCellsFromArray(grid, Coord(2, 0)) shouldBe List(
        Cell(".", Coord(1, 0)),
        Cell(".", Coord(1, 1)),
        Cell(".", Coord(2, 1)),
      )

    }

  }

  "isPartNumber" should {

    "return false if diagonal cell are all dots or numbers" in {

      day3.isPartNumber(grid, Coord(1, 1)) shouldBe false

    }

    "return true if diagonal cell has an asterisk" in {
      day3.isPartNumber(grid, Coord(0, 3)) shouldBe true
    }

    "return true if diagonal cell has a hash" in {
      val newGrid = replaceAsteriskInGrid("#")
      day3.isPartNumber(newGrid, Coord(0, 3)) shouldBe true
    }

    "return true if diagonal cell has a dollar" in {
      val newGrid = replaceAsteriskInGrid("$")
      day3.isPartNumber(newGrid, Coord(0, 3)) shouldBe true
    }

    "return true if diagonal cell has a plus" in {
      val newGrid = replaceAsteriskInGrid("+")
      day3.isPartNumber(newGrid, Coord(0, 3)) shouldBe true
    }

    "return true if diagonal cell has a slash" in {
      val newGrid = replaceAsteriskInGrid("+")
      day3.isPartNumber(newGrid, Coord(0, 3)) shouldBe true
    }

  }

  "findPartNumbers" should {

    "find and create entries for numbers" in {

      day3.findPartNumbers(grid) shouldBe List(
        PartNumber(467, isPart = true, coords = List(Coord(0, 0), Coord(0, 1), Coord(0, 2))),
        PartNumber(114, isPart = false, coords = List(Coord(0, 5), Coord(0, 6), Coord(0, 7))),
        PartNumber(35, isPart = true, coords = List(Coord(2, 2), Coord(2, 3))),
        PartNumber(633, isPart = false, coords = List(Coord(2, 6), Coord(2, 7), Coord(2, 8)))
      )

    }

    "parse the grid when numbers at end of line" in {

      val grid: List[List[Cell]] =
        List(
          List(
            Cell("4", Coord(0, 0)),
            Cell("6", Coord(0, 1)),
            Cell("7", Coord(0, 2)),
            Cell(".", Coord(0, 3)),
            Cell(".", Coord(0, 4)),
            Cell("1", Coord(0, 5)),
            Cell(".", Coord(0, 6)),
            Cell(".", Coord(0, 7)),
            Cell("1", Coord(0, 8)),
            Cell("4", Coord(0, 9))
          ),
          List(
            Cell(".", Coord(1, 0)),
            Cell(".", Coord(1, 1)),
            Cell(".", Coord(1, 2)),
            Cell("*", Coord(1, 3)),
            Cell(".", Coord(1, 4)),
            Cell(".", Coord(1, 5)),
            Cell(".", Coord(1, 6)),
            Cell(".", Coord(1, 7)),
            Cell(".", Coord(1, 8)),
            Cell(".", Coord(1, 9))
          ),
          List(
            Cell(".", Coord(2, 0)),
            Cell(".", Coord(2, 1)),
            Cell("3", Coord(2, 2)),
            Cell("5", Coord(2, 3)),
            Cell(".", Coord(2, 4)),
            Cell(".", Coord(2, 5)),
            Cell(".", Coord(2, 6)),
            Cell("6", Coord(2, 7)),
            Cell("3", Coord(2, 8)),
            Cell("3", Coord(2, 9))
          )
        )


      day3.findPartNumbers(grid) shouldBe List(
        PartNumber(467, isPart = true, coords = List(Coord(0, 0), Coord(0, 1), Coord(0, 2))),
        PartNumber(1, isPart = false, coords = List(Coord(0, 5))),
        PartNumber(14, isPart = false, coords = List(Coord(0, 8), Coord(0, 9))),
        PartNumber(35, isPart = true, coords = List(Coord(2, 2), Coord(2, 3))),
        PartNumber(633, isPart = false, coords = List(Coord(2, 7), Coord(2, 8), Coord(2, 9)))
      )
    }

  }

  "sumPartNumbers" should {

    "sum only the part numbers of parts and not of non-parts" in {

      val parts = day3.findPartNumbers(grid)
      day3.sumPartNumbers(parts) shouldBe 502

    }

    "produce the example result from the question" in {
      val gridString = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
      val grid = day3.parseTable(gridString)
      val parts = day3.findPartNumbers(grid)

      day3.sumPartNumbers(parts) shouldBe 4361
    }
  }

  "findSumOfGearMultiplierOfGrid" should {
    "produce the example result from the question" in {
      val gridString = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
      val grid = day3.parseTable(gridString)
      val parts = day3.findPartNumbers(grid)

      day3.findSumOfGearMultiplierOfGrid(grid, parts) shouldBe 467835
    }
  }

  "isGear" should {

    "return false if cell is a dot" in {
      day3.isGear(gridForTestingGearNumbers, Cell(".", Coord(1, 1))) shouldBe false
    }

    "return false if cell is a number" in {
      day3.isGear(gridForTestingGearNumbers, Cell("4", Coord(0, 0))) shouldBe false
    }

    "return false if diagonal cell are all dots" in {
      day3.isGear(gridForTestingGearNumbers, Cell("*", Coord(2, 0))) shouldBe false
    }

    "return false if only one diagonal is a number" in {
      day3.isGear(gridForTestingGearNumbers, Cell("*", Coord(1, 9))) shouldBe false
    }

    "return true if two part numbers diagonally adjacent" in {
      day3.isGear(gridForTestingGearNumbers, Cell("*", Coord(1, 3))) shouldBe true
    }

    "return false if diagonally adjacent to two digits of the same part " in {
      day3.isGear(gridForTestingGearNumbers, Cell("*", Coord(1, 0))) shouldBe false
    }

  }

  "calculateGearMultiplier" should {

    "multiply together the two adjacent numbers" in {

      val part1 = PartNumber(467, isPart = true, coords = List(Coord(0, 0), Coord(0, 1), Coord(0, 2)))
      val part2 = PartNumber(114, isPart = false, coords = List(Coord(0, 5), Coord(0, 6), Coord(0, 7)))

      day3.calculateGearMultiplier(List(part1, part2)) shouldBe 53238

    }

  }

  "getAdjacentPartNumbers" should {

    "return part numbers diagonally left and right above" in {

      val partNos = List(
        PartNumber(467, isPart = true, coords = List(Coord(0, 0), Coord(0, 1), Coord(0, 2))),
        PartNumber(114, isPart = false, coords = List(Coord(0, 4), Coord(0, 5), Coord(0, 6))),
      )

      day3.getAdjacentPartNumbers(partNos, Coord(1, 3)) shouldBe partNos

    }

    "return part numbers diagonally left and right below" in {

      val partNos = List(
        PartNumber(467, isPart = true, coords = List(Coord(2, 0), Coord(2, 1), Coord(2, 2))),
        PartNumber(114, isPart = false, coords = List(Coord(2, 4), Coord(2, 5), Coord(2, 6))),
      )

      day3.getAdjacentPartNumbers(partNos, Coord(1, 3)) shouldBe partNos

    }

    "return part numbers right above and below and left and right" in {

      val partNos = List(
        PartNumber(467, isPart = true, coords = List(Coord(0, 1), Coord(0, 2), Coord(0, 3))),
        PartNumber(114, isPart = false, coords = List(Coord(2, 1), Coord(2, 2), Coord(2, 3))),
        PartNumber(5, isPart = false, coords = List(Coord(1, 0))),
        PartNumber(11, isPart = false, coords = List(Coord(1, 2), Coord(1, 3)))
      )

      day3.getAdjacentPartNumbers(partNos, Coord(1, 1)) shouldBe partNos

    }


  }

  private def replaceAsteriskInGrid(replaceWith: String) = {
    grid.map {
      line: List[Cell] =>
        line.map {
          case x if x.value == "*" => Cell(replaceWith, x.coord)
          case x: Cell => x
        }
    }
  }

}
