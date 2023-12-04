package aoc.day1

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class Day1Spec extends AnyWordSpec with Matchers {

  private val day1 = Day1()

  "getFirstAndLastDigit" should {
    "get digits when numbers" in {
      day1.getFirstAndLastDigit("1abc2") shouldBe Seq(1, 2)
      day1.getFirstAndLastDigit("pqr3stu8vwx") shouldBe Seq(3, 8)
      day1.getFirstAndLastDigit("a1b2c3d4e5f") shouldBe Seq(1, 5)
    }

    "repeat digit if only one" in {
      day1.getFirstAndLastDigit("treb7uchet") shouldBe Seq(7, 7)
    }

    "find digits that are words" in {
      day1.getFirstAndLastDigit("oneabc") shouldBe Seq(1, 1)
      day1.getFirstAndLastDigit("two7sdfthreesdf") shouldBe Seq(2, 3)
      day1.getFirstAndLastDigit("fourfivesixseveneightnine") shouldBe Seq(4, 9)

      // Interesting part to parse here is the "twone" at the end
      day1.getFirstAndLastDigit("honemkmbfbnlhtbq19twonekbp") shouldBe Seq(1, 1)

      //Interesting part to parse here is the "eightwo" at the start
      day1.getFirstAndLastDigit("eightwothree") shouldBe Seq(8, 3)

      //Interesting part to parse here is the "eightwo" at the start
      day1.getFirstAndLastDigit("eightwothree") shouldBe Seq(8, 3)

      // Interesting part to parse here is the "twone" at the start
      day1.getFirstAndLastDigit("xtwone3four") shouldBe Seq(2, 4)

    }

  }

  "create number from set" should {

    "combine two digit seq to make a number" in {

      day1.createNumberFromSet(Seq(1, 2)) shouldBe 12


    }

  }

  "calculate Calibration Value" should {

    "sum up the numeric values of each line" in {

      val input = "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet"

      day1.calculateCalibrationValue(input) shouldBe 142

    }

    "sum up the numeric values of each line with strings" in {

      val input = "two1nine\neightwothree\nabcone2threexyz\nxtwone3four" +
        "\n4nineeightseven2\nzoneight234\n7pqrstsixteen"

      day1.calculateCalibrationValue(input) shouldBe 281

    }

  }

}
