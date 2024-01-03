package aoc.day10

import scala.annotation.tailrec
import scala.io.Source

case class Day10() {
  def findStepsFurthestPoint(loop: String): Int = {
    val loopSize = findMainLoopSize(loop)
    Math.round(loopSize / 2)
  }

  def findNextCellInLoop(grid: Map[(Int, Int), String], currentCell: (Int, Int), currentValue: String, visitedAlready: List[(Int, Int)]): (Int, Int) = {

    val right = (currentCell._1 + 1, currentCell._2)
    val left = (currentCell._1 - 1, currentCell._2)
    val above = (currentCell._1, currentCell._2 - 1)
    val below = (currentCell._1, currentCell._2 + 1)

    val rightVal = grid.getOrElse(right, ".")
    val belowVal = grid.getOrElse(below, ".")
    val leftVal = grid.getOrElse(left, ".")
    val aboveVal = grid.getOrElse(above, ".")

    val visitedAlreadyRight = visitedAlready.contains(right)
    val visitedAlreadyLeft = visitedAlready.contains(left)
    val visitedAlreadyAbove = visitedAlready.contains(above)
    val visitedAlreadyBelow = visitedAlready.contains(below)

    val visitedAlreadySize = visitedAlready.length

    currentValue match {
      case "S" =>
        if (!visitedAlreadyRight && (rightVal == "-" || rightVal == "J" || rightVal == "7")) {
          right
        } else if (!visitedAlreadyBelow && (belowVal == "|" || belowVal == "L" || belowVal == "J")) {
          below
        } else if (!visitedAlreadyLeft && (leftVal == "-" || leftVal == "L" || leftVal == "F")) {
          left
        } else if (!visitedAlreadyAbove && (aboveVal == "|" || aboveVal == "7" || aboveVal == "F")) {
          above
        } else {
          currentCell
        }

      case "-" =>
        if (rightVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyRight && (rightVal == "-" || rightVal == "J" || rightVal == "7"))) {
          right
        } else if (leftVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyLeft && (leftVal == "-" || leftVal == "F" || leftVal == "L"))) {
          left
        } else {
          currentCell
        }

      case "|" =>
        if (belowVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyBelow && (belowVal == "|" || belowVal == "J" || belowVal == "L"))) {
          below
        } else if (aboveVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyAbove && (aboveVal == "|" || aboveVal == "F" || aboveVal == "7"))) {
          above
        } else {
          currentCell
        }

      case "L" =>
        if (rightVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyRight && (rightVal == "-" || rightVal == "J" || rightVal == "7"))) {
          right
        } else if (aboveVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyAbove && (aboveVal == "|" || aboveVal == "7" || aboveVal == "F"))) {
          above
        } else {
          currentCell
        }

      case "J" =>
        if (leftVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyLeft && (leftVal == "-" || leftVal == "L" || leftVal == "F"))) {
          left
        } else if (aboveVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyAbove && (aboveVal == "|" || aboveVal == "7" || aboveVal == "F"))) {
          above
        } else {
          currentCell
        }

      case "7" =>
        if (belowVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyBelow && (belowVal == "|" || belowVal == "L" || belowVal == "J"))) {
          below
        } else if (leftVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyLeft && (leftVal == "-" || leftVal == "L" || leftVal == "F"))) {
          left
        } else {
          currentCell
        }

      case "F" =>
        if (rightVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyRight && (rightVal == "-" || rightVal == "J" || rightVal == "7"))) {
          right
        } else if (belowVal == "S" && visitedAlreadySize > 2 | (!visitedAlreadyBelow && (belowVal == "|" || belowVal == "L" || belowVal == "J"))) {
          below
        } else {
          currentCell
        }

      case _ => currentCell

    }
  }

  @tailrec
  private def findMainLoop(grid: Map[(Int, Int), String], startCell: ((Int, Int), String),
                           currentCell: ((Int, Int), String), loopList: List[(Int, Int)]): Int = {
    if (currentCell == startCell) {
      loopList.size
    } else {
      val nextCell = findNextCellInLoop(grid, currentCell._1, currentCell._2, loopList)
      findMainLoop(grid, startCell, nextCell -> grid(nextCell), loopList :+ nextCell)
    }
  }

  def findMainLoopSize(loop: String): Int = {

    val grid = createGrid(loop)

    val start = grid.find(x => x._2 == "S").get
    val next = findNextCellInLoop(grid, start._1, start._2, List(start._1))

    //Take away 1 for the duplicate counting of the start and end
    findMainLoop(grid, start, next -> grid(next), List(start._1, next)) - 1

  }

  def createGrid(entry: String): Map[(Int, Int), String] = {
    entry.split("\n").zipWithIndex
      .foldLeft(Map.empty[(Int, Int), String]) { (cur, entry) =>
        cur ++ entry._1.split("").zipWithIndex
          .foldLeft(Map.empty[(Int, Int), String]) { (cur2, entry2) =>
            cur2 ++ Map((entry2._2, entry._2) -> entry2._1)
          }
      }
  }

  def isPipe(tile: String): Boolean = tile.matches("[\\|\\-FJ7LS]")

  def run(): Unit = {

    val file = Source.fromResource("day10Input.txt").getLines().mkString("\n")

    val answer = findStepsFurthestPoint(file)

    println(s"Answer is $answer")

  }
}
