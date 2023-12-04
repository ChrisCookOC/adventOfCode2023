package aoc.day1

import java.net.URL
import scala.io.{BufferedSource, Source}

case class Day1() {
  def createNumberFromSet(list: Seq[Int]): Int = list.mkString("").toInt

  def getFirstAndLastDigit(str: String): Seq[Int] = {

    val regex = "one|two|three|four|five|six|seven|eight|nine|\\d".r
    val backwardsRegex = "eno|owt|eerht|ruof|evif|xis|neves|thgie|enin|\\d".r

    val firstDigit = regex.findFirstIn(str).map {
      case "one" => 1
      case "two" => 2
      case "three" => 3
      case "four" => 4
      case "five" => 5
      case "six" => 6
      case "seven" => 7
      case "eight" => 8
      case "nine" => 9
      case x => x.toInt
    }.getOrElse(0)

    val secondDigit = backwardsRegex.findFirstIn(str.reverse).map {
      case "eno" => 1
      case "owt" => 2
      case "eerht" => 3
      case "ruof" => 4
      case "evif" => 5
      case "xis" => 6
      case "neves" => 7
      case "thgie" => 8
      case "enin" => 9
      case x => x.toInt
    }.getOrElse(0)

    Seq(firstDigit, secondDigit)

  }

  def calculateCalibrationValue(input: String): Int = {

    val entries = input.split("\n")

    entries.map(line => {
      createNumberFromSet(getFirstAndLastDigit(line))
    }).sum

  }

  def run(): Unit = {

    val file = getClass.getResourceAsStream("input.txt")

    val input = Source.fromInputStream(file).mkString

    val result = calculateCalibrationValue(input)

    println(s"Calibration Value is $result")
  }

}
