package aoc.day3

import scala.io.Source

case class Day3() {
  def findSumOfPartNumbersInGrid(gridString: String): Int = {
    val grid = parseTable(gridString)
    sumPartNumbers(grid)
  }

  def sumPartNumbers(grid: Array[Array[String]]): Int = {
    val parts = findPartNumbers(grid)
    parts.collect {
      case p if p.isPart => p.number
    }.sum
  }

  def isPartNumber(grid: Array[Array[String]], coords: (Int, Int)): Boolean = {

    val list = getDiagonalCellsFromArray(grid, coords)

    ".*[$*/#@%=+&-].*".r.matches(list.toString)

  }

  def getDiagonalCellsFromArray(grid: Array[Array[String]], coords: (Int, Int)): List[String] = {

    val lineCount = grid.length - 1
    val lineSize = grid.head.length - 1

    if (coords._1 == 0 && coords._2 == 0) {
      List(
        grid(coords._1)(coords._2 + 1),
        grid(coords._1 + 1)(coords._2),
        grid(coords._1 + 1)(coords._2 + 1)
      )
    } else if (coords._1 == lineCount && coords._2 == lineSize) {
      List(
        grid(coords._1 - 1)(coords._2 - 1),
        grid(coords._1 - 1)(coords._2),
        grid(coords._1)(coords._2 - 1),
      )
    }
    else if (coords._1 == 0 && coords._2 == lineSize) {
      List(
        grid(coords._1)(coords._2 - 1),
        grid(coords._1 + 1)(coords._2 - 1),
        grid(coords._1 + 1)(coords._2),
      )
    } else if (coords._1 == lineCount && coords._2 == 0) {
      List(
        grid(coords._1 - 1)(coords._2),
        grid(coords._1 - 1)(coords._2 + 1),
        grid(coords._1)(coords._2 + 1),
      )
    }

    else if (coords._1 == 0) {
      List(
        grid(coords._1)(coords._2 - 1),
        grid(coords._1)(coords._2 + 1),
        grid(coords._1 + 1)(coords._2 - 1),
        grid(coords._1 + 1)(coords._2),
        grid(coords._1 + 1)(coords._2 + 1))
    }
    else if (coords._2 == 0) {
      List(
        grid(coords._1 - 1)(coords._2),
        grid(coords._1 - 1)(coords._2 + 1),
        grid(coords._1)(coords._2 + 1),
        grid(coords._1 + 1)(coords._2),
        grid(coords._1 + 1)(coords._2 + 1)
      )
    }
    else if (coords._1 == lineCount) {
      List(
        grid(coords._1 - 1)(coords._2 - 1),
        grid(coords._1 - 1)(coords._2),
        grid(coords._1 - 1)(coords._2 + 1),
        grid(coords._1)(coords._2 - 1),
        grid(coords._1)(coords._2 + 1),
      )
    }
    else if (coords._2 == lineSize) {
      List(
        grid(coords._1 - 1)(coords._2 - 1),
        grid(coords._1 - 1)(coords._2),
        grid(coords._1)(coords._2 - 1),
        grid(coords._1 + 1)(coords._2 - 1),
        grid(coords._1 + 1)(coords._2),
      )
    }
    else {
      List(
        grid(coords._1 - 1)(coords._2 - 1),
        grid(coords._1 - 1)(coords._2),
        grid(coords._1 - 1)(coords._2 + 1),
        grid(coords._1)(coords._2 - 1),
        grid(coords._1)(coords._2 + 1),
        grid(coords._1 + 1)(coords._2 - 1),
        grid(coords._1 + 1)(coords._2),
        grid(coords._1 + 1)(coords._2 + 1)
      )
    }

  }

  def findPartNumbers(grid: Array[Array[String]]): List[PartNumber] = {

    //TODO would be nice if this was more functional
    var numbers = List.empty[PartNumber]

    grid.zipWithIndex.foreach { case (line, lineNo) =>

      var currentNumber = ""
      var isPart = false

      line.zipWithIndex.foreach { case (item, entry) =>
        if ("\\d".r.matches(item)) {
          currentNumber = currentNumber ++ item
          isPart = isPart || isPartNumber(grid, (lineNo, entry))
        } else {
          if (currentNumber != "") {
            numbers = numbers :+ PartNumber(currentNumber.toInt, isPart)
            currentNumber = ""
            isPart = false
          }
        }

      }

      if (currentNumber != "") {
        numbers = numbers :+ PartNumber(currentNumber.toInt, isPart)
        currentNumber = ""
        isPart = false
      }


    }
    numbers
  }

  def parseTable(table: String): Array[Array[String]] = {
    val lines = table.split("\n")

    lines.map(line => parseLine(line))

  }

  def parseLine(line: String): Array[String] = line.split("")

  def run(): Unit = {

    val file = getClass.getResourceAsStream("input.txt")

    val input = Source.fromInputStream(file).mkString

    val partSum = findSumOfPartNumbersInGrid(input)

    println(s"Sum of part numbers is $partSum")

  }
}

case class PartNumber(number: Int, isPart: Boolean)
