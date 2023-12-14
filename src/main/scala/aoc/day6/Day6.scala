package aoc.day6

import scala.io.Source

case class Day6() {
  def processInput(input: String): Int = {
    val inputProcessed = input.split("\n")

    val time = parseTime(inputProcessed.head)
    val dist = parseDistance(inputProcessed.last)

    Range.inclusive(0, time.length - 1)
      .map(x => howManyWaysCanWin(time(x), dist(x)))
      .product
  }

  def howManyWaysCanWin(maxTime: Int, distanceToBeat: Int): Int = {

    Range.inclusive(0, maxTime).map(x => getDistance(x, maxTime)).count(x => x > distanceToBeat)
  }

  def getDistance(timeHoldingButton: Int, totalTime: Int): Int = {
    timeHoldingButton * (totalTime - timeHoldingButton)
  }

  def parseDistance(distance: String): List[Int] = {
    distance.split(" +").tail.map(_.toInt).toList
  }

  def parseTime(time: String): List[Int] = {
    time.split(" +").tail.map(_.toInt).toList
  }

  def run(): Unit = {

    val file = getClass.getResourceAsStream("input.txt")

    val input = Source.fromInputStream(file).mkString

    val total = processInput(input)

    println(s"Product of ways to win is $total")

  }

}
