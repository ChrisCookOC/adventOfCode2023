package aoc.day1

import java.net.URL
import scala.io.{BufferedSource, Source}

case class Day1() {
  def createNumberFromSet(list: Seq[Int]): Int = list.mkString("").toInt

  def getFirstAndLastDigitFromList(list: Seq[Int]): Seq[Int] =
    if (list.nonEmpty) Seq(list.head, list.last) else Seq.empty

  def getDigitsFromLine(str: String): Seq[Int] = {
    str.collect { case x if x.isDigit => x.toString.toInt}
  }

  def calculateCalibrationValue(input: String): Int = {

    val entries = input.split("\n")

    entries.map(line => createNumberFromSet(getFirstAndLastDigitFromList(getDigitsFromLine(line)))).sum

  }

  def run(): Unit = {

    val file = getClass.getResourceAsStream("input.txt")

    val input = Source.fromInputStream(file).mkString

    val result = calculateCalibrationValue(input)

    println(s"Calibration Value is $result")
  }

}
