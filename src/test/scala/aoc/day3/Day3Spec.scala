package aoc.day3

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day3Spec extends AnyWordSpec with Matchers {

  private val day3 = Day3()

  private val grid = Array(
    Array("4", "6", "7", ".", ".", "1", "1", "4", ".", "."),
    Array(".", ".", ".", "*", ".", ".", ".", ".", ".", "."),
    Array(".", ".", "3", "5", ".", ".", "6", "3", "3", ".")
  )

  "parseLine" should {

    "parse line into array" in {

      day3.parseLine("467..114..") shouldBe Array("4", "6", "7", ".", ".", "1", "1", "4", ".", ".")
      day3.parseLine("617*......") shouldBe Array("6", "1", "7", "*", ".", ".", ".", ".", ".", ".")

    }

  }

  "parseGrid" should {
    "parse the grid into a 2D array" in {

      val table = "467..114..\n...*......\n..35..633."

      day3.parseTable(table) shouldBe grid

    }

    "parse the grid when numbers at end of line" in {
      val table = "467..1..14\n...*......\n..35...633"

      val expectedGrid = Array(
        Array("4", "6", "7", ".", ".", "1", ".", ".", "1", "4"),
        Array(".", ".", ".", "*", ".", ".", ".", ".", ".", "."),
        Array(".", ".", "3", "5", ".", ".", ".", "6", "3", "3")
      )

      day3.parseTable(table) shouldBe expectedGrid
    }

  }

  "getDiagonalCellsFromArray" should {

    "get diagonals from a given index" in {
      day3.getDiagonalCellsFromArray(grid, (1, 1)) shouldBe List("4", "6", "7", ".", ".", ".", ".", "3")
    }

    "not go negative" in {

      day3.getDiagonalCellsFromArray(grid, (0, 1)) shouldBe List("4", "7", ".", ".", ".")
      day3.getDiagonalCellsFromArray(grid, (1, 0)) shouldBe List("4", "6", ".", ".", ".")

    }

    "not go off the grid" in {

      day3.getDiagonalCellsFromArray(grid, (2, 9)) shouldBe List(".", ".", "3")
      day3.getDiagonalCellsFromArray(grid, (2, 1)) shouldBe List(".", ".", ".", ".", "3")
      day3.getDiagonalCellsFromArray(grid, (1, 9)) shouldBe List(".", ".", ".", "3", ".")

    }

    "handle the corners" in {
      day3.getDiagonalCellsFromArray(grid, (0, 0)) shouldBe List("6", ".", ".")
      day3.getDiagonalCellsFromArray(grid, (0, 9)) shouldBe List(".", ".", ".")
      day3.getDiagonalCellsFromArray(grid, (2, 9)) shouldBe List(".", ".", "3")
      day3.getDiagonalCellsFromArray(grid, (2, 0)) shouldBe List(".", ".", ".")

    }

  }

  "isPartNumber" should {

    "return false if diagonal cell are all dots or numbers" in {

      day3.isPartNumber(grid, (1, 1)) shouldBe false

    }

    "return true if diagonal cell has an asterisk" in {
      day3.isPartNumber(grid, (0, 3)) shouldBe true
    }

    "return true if diagonal cell has a hash" in {
      val newGrid = replaceAsteriskInGrid("#")
      day3.isPartNumber(newGrid, (0, 3)) shouldBe true
    }

    "return true if diagonal cell has a dollar" in {
      val newGrid = replaceAsteriskInGrid("$")
      day3.isPartNumber(newGrid, (0, 3)) shouldBe true
    }

    "return true if diagonal cell has a plus" in {
      val newGrid = replaceAsteriskInGrid("+")
      day3.isPartNumber(newGrid, (0, 3)) shouldBe true
    }

    "return true if diagonal cell has a slash" in {
      val newGrid = replaceAsteriskInGrid("+")
      day3.isPartNumber(newGrid, (0, 3)) shouldBe true
    }

  }

  "findPartNumbers" should {

    "find and create entries for numbers" in {

      day3.findPartNumbers(grid) shouldBe List(
        PartNumber(467, isPart = true),
        PartNumber(114, isPart = false),
        PartNumber(35, isPart = true),
        PartNumber(633, isPart = false),
      )

    }

    "parse the grid when numbers at end of line" in {

      val grid = Array(
        Array("4", "6", "7", ".", ".", "1", ".", ".", "1", "4"),
        Array(".", ".", ".", "*", ".", ".", ".", ".", ".", "."),
        Array(".", ".", "3", "5", ".", ".", ".", "6", "3", "3")
      )

      day3.findPartNumbers(grid) shouldBe List(
        PartNumber(467, isPart = true),
        PartNumber(1, isPart = false),
        PartNumber(14, isPart = false),
        PartNumber(35, isPart = true),
        PartNumber(633, isPart = false),
      )
    }

  }

  "sumPartNumbers" should {

    "sum only the part numbers of parts and not of non-parts" in {

      day3.sumPartNumbers(grid) shouldBe 502

    }

  }

  "findSumOfPartNumbersInGrid" should {
    "produce the example result from the question" in {
      val grid = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."
      day3.findSumOfPartNumbersInGrid(grid) shouldBe 4361
    }
  }

  private def replaceAsteriskInGrid(replaceWith: String) = {
    grid.map {
      line: Array[String] =>
        line.map {
          case "*" => replaceWith
          case x: String => x
        }
    }
  }

}
