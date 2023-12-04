package aoc.day1

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class Day1Spec extends AnyWordSpec with Matchers {

  private val day1 = Day1()

  "get digits from line" should {
    "get all digits from line" in {
      day1.getDigitsFromLine("1abc2") shouldBe Seq(1, 2)
      day1.getDigitsFromLine("pqr3stu8vwx") shouldBe Seq(3, 8)
      day1.getDigitsFromLine("a1b2c3d4e5f") shouldBe Seq(1,2,3,4,5)
      day1.getDigitsFromLine("treb7uchet") shouldBe Seq(7)
    }
  }

  "getFirstAndLastDigit" should {

    "return both digits if two" in {
      day1.getFirstAndLastDigitFromList(Seq(1,2)) shouldBe Seq(1,2)
    }

    "return first and last digits if three or more" in {
      day1.getFirstAndLastDigitFromList(Seq(1, 2, 3)) shouldBe Seq(1, 3)
      day1.getFirstAndLastDigitFromList(Seq(7, 3, 1, 54, 342)) shouldBe Seq(7, 342)
    }

    "return same digits if just 1" in {
      day1.getFirstAndLastDigitFromList(Seq(43)) shouldBe Seq(43, 43)
    }

    "return empty if no numbers" in {
      day1.getFirstAndLastDigitFromList(Seq.empty) shouldBe Seq.empty
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

  }

}
