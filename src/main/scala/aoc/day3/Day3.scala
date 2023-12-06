package aoc.day3

import scala.io.Source

case class Day3() {
  def findSumOfGearMultiplierOfGrid(grid: List[List[Cell]], parts: List[PartNumber]): Int = {

    grid.foldLeft(0)((sum: Int, row: List[Cell]) => {
      sum + row.foldLeft(0)((sum, cell) => {
        if (isGear(grid, cell)) {
          val listOfParts = getAdjacentPartNumbers(parts, cell.coord)
          println(s"Adjacent parts for gear at ${cell.coord} are ${listOfParts.map(x => x.number).mkString(",")}")

          sum + calculateGearMultiplier(listOfParts)
        } else {
          sum
        }
      }
      )
    })

  }

  def calculateGearMultiplier(part: List[PartNumber]): Int = {
    part.foldLeft(1)((product, part) => product * part.number)
  }

  def getAdjacentPartNumbers(partNos: List[PartNumber], coordToGetFor: Coord): List[PartNumber] = {

    partNos.filter(part =>
      part.coords.contains(Coord(coordToGetFor.row - 1, coordToGetFor.col - 1))
        || part.coords.contains(Coord(coordToGetFor.row - 1, coordToGetFor.col + 1))
        || part.coords.contains(Coord(coordToGetFor.row - 1, coordToGetFor.col))
        || part.coords.contains(Coord(coordToGetFor.row + 1, coordToGetFor.col))
        || part.coords.contains(Coord(coordToGetFor.row, coordToGetFor.col - 1))
        || part.coords.contains(Coord(coordToGetFor.row, coordToGetFor.col + 1))
        || part.coords.contains(Coord(coordToGetFor.row + 1, coordToGetFor.col - 1))
        || part.coords.contains(Coord(coordToGetFor.row + 1, coordToGetFor.col + 1))
    )

  }

  def sumPartNumbers(parts: List[PartNumber]): Int = {
    parts.collect {
      case p if p.isPart => p.number
    }.sum
  }

  def isPartNumber(grid: List[List[Cell]], coords: Coord): Boolean = {

    val list = getDiagonalCellsFromArray(grid, coords)

    val valuesAsString = list.foldLeft("") { (str, cell) => str + cell.value }

    ".*[$*/#@%=+&-].*".r.matches(valuesAsString)

  }

  def isGear(grid: List[List[Cell]], cell: Cell): Boolean = {

    if (cell.value != "*") false
    else {

      // println(s"WE HAVE A *: coords ${cell.coord.row}, ${cell.coord.col}, ${cell.value}")

      val list = getDiagonalCellsFromArray(grid, cell.coord)

      //      println(s"PICTURE TIME")
      //      var lastCol = 0
      //      var lastRow = 0
      //      var picture = ""
      //      list.foreach(x => {
      //
      //        if (lastRow != x.coord.row) {
      //          picture = s"$picture\n${x.value}"
      //          lastCol = x.coord.col
      //          lastRow = x.coord.row
      //        } else if (lastCol != x.coord.col - 1) {
      //          picture = picture + "*" + x.value
      //          lastCol = x.coord.col
      //        } else {
      //          picture = picture + x.value
      //          lastCol = x.coord.col
      //        }
      //
      //      })
      //
      //      println(picture)

      var lastCell: Cell = null
      val intCount = list.foldLeft(0)((cur, item) => {
        if (item.value.matches("\\d")) {
          if (lastCell == null || lastCell.coord.row != item.coord.row || lastCell.coord.col + 1 != item.coord.col) {
            lastCell = item
            cur + 1
          } else if (lastCell.coord.col + 1 == item.coord.col) {
            lastCell = item
            cur
          } else {
            cur
          }
        } else {
          cur
        }
      })

      println(s"INT COUNT: $intCount")
      intCount > 1

    }

  }

  def getDiagonalCellsFromArray(grid: List[List[Cell]], coords: Coord): List[Cell] = {

    val lineCount = grid.length - 1
    val lineSize = grid.head.length - 1

    if (coords.row == 0 && coords.col == 0) {
      List(
        grid(coords.row)(coords.col + 1),
        grid(coords.row + 1)(coords.col),
        grid(coords.row + 1)(coords.col + 1)
      )
    } else if (coords.row == lineCount && coords.col == lineSize) {
      List(
        grid(coords.row - 1)(coords.col - 1),
        grid(coords.row - 1)(coords.col),
        grid(coords.row)(coords.col - 1),
      )
    }
    else if (coords.row == 0 && coords.col == lineSize) {
      List(
        grid(coords.row)(coords.col - 1),
        grid(coords.row + 1)(coords.col - 1),
        grid(coords.row + 1)(coords.col),
      )
    } else if (coords.row == lineCount && coords.col == 0) {
      List(
        grid(coords.row - 1)(coords.col),
        grid(coords.row - 1)(coords.col + 1),
        grid(coords.row)(coords.col + 1),
      )
    }

    else if (coords.row == 0) {
      List(
        grid(coords.row)(coords.col - 1),
        grid(coords.row)(coords.col + 1),
        grid(coords.row + 1)(coords.col - 1),
        grid(coords.row + 1)(coords.col),
        grid(coords.row + 1)(coords.col + 1))
    }
    else if (coords.col == 0) {
      List(
        grid(coords.row - 1)(coords.col),
        grid(coords.row - 1)(coords.col + 1),
        grid(coords.row)(coords.col + 1),
        grid(coords.row + 1)(coords.col),
        grid(coords.row + 1)(coords.col + 1)
      )
    }
    else if (coords.row == lineCount) {
      List(
        grid(coords.row - 1)(coords.col - 1),
        grid(coords.row - 1)(coords.col),
        grid(coords.row - 1)(coords.col + 1),
        grid(coords.row)(coords.col - 1),
        grid(coords.row)(coords.col + 1),
      )
    }
    else if (coords.col == lineSize) {
      List(
        grid(coords.row - 1)(coords.col - 1),
        grid(coords.row - 1)(coords.col),
        grid(coords.row)(coords.col - 1),
        grid(coords.row + 1)(coords.col - 1),
        grid(coords.row + 1)(coords.col),
      )
    }
    else {
      List(
        grid(coords.row - 1)(coords.col - 1),
        grid(coords.row - 1)(coords.col),
        grid(coords.row - 1)(coords.col + 1),
        grid(coords.row)(coords.col - 1),
        grid(coords.row)(coords.col + 1),
        grid(coords.row + 1)(coords.col - 1),
        grid(coords.row + 1)(coords.col),
        grid(coords.row + 1)(coords.col + 1)
      )
    }

  }

  def findPartNumbers(grid: List[List[Cell]]): List[PartNumber] = {

    //TODO would be nice if this was more functional
    var numbers = List.empty[PartNumber]

    grid.foreach { line =>

      var currentNumber = ""
      var isPart = false
      var coordList = List.empty[Coord]

      line.foreach { cell =>
        if ("\\d".r.matches(cell.value)) {
          currentNumber = currentNumber ++ cell.value
          coordList = coordList :+ cell.coord
          isPart = isPart || isPartNumber(grid, cell.coord)
        } else {
          if (currentNumber != "") {
            numbers = numbers :+ PartNumber(currentNumber.toInt, isPart, coordList)
            currentNumber = ""
            coordList = List.empty
            isPart = false
          }
        }

      }

      if (currentNumber != "") {
        numbers = numbers :+ PartNumber(currentNumber.toInt, isPart, coordList)
        currentNumber = ""
        coordList = List.empty
        isPart = false
      }


    }

    numbers
  }

  def parseTable(table: String): List[List[Cell]] = {
    val lines = table.split("\n")

    lines.zipWithIndex.map(line => parseLine(line)).toList

  }

  def parseLine(line: (String, Int)): List[Cell] = {
    val values = line._1.split("").zipWithIndex
    values.map(entry => Cell(entry._1, Coord(line._2, entry._2))).toList
  }

  def run(): Unit = {

    val file = getClass.getResourceAsStream("input.txt")

    val input = Source.fromInputStream(file).mkString

    val grid = parseTable(input)
    val parts = findPartNumbers(grid)

    val partSum = sumPartNumbers(parts)
    val gearMultiplier = findSumOfGearMultiplierOfGrid(grid, parts)

    println(s"Sum of part numbers is $partSum")
    println(s"Sum of gear multipliers is $gearMultiplier")

  }
}

case class PartNumber(number: Int, isPart: Boolean, coords: List[Coord])

case class Cell(value: String, coord: Coord)

case class Coord(row: Int, col: Int)