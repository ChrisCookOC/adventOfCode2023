package aoc.day6

case class Day6() {
  def processInput(input: String): Long = {
    val inputProcessed = input.split("\n")

    val time = parseTime(inputProcessed.head)
    val dist = parseDistance(inputProcessed.last)

    howManyWaysCanWin(time, dist)
  }

  def howManyWaysCanWin(maxTime: Long, distanceToBeat: Long): Long = {

    Range.Long.inclusive(0, maxTime, 1).map(x => getDistance(x, maxTime)).count(x => x > distanceToBeat)
  }

  def getDistance(timeHoldingButton: Long, totalTime: Long): Long = {
    timeHoldingButton * (totalTime - timeHoldingButton)
  }

  def parseDistance(distance: String): Long = {
    distance.split(" +").tail.mkString("").toLong
  }

  def parseTime(time: String): Long = {
    time.split(" +").tail.mkString("").toLong
  }

  def run(): Unit = {

    val input = Input.get

    val total = processInput(input)

    println(s"Product of ways to win is $total")

  }

}
