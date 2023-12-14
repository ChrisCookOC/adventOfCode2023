package aoc.day6

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day6Spec extends AnyWordSpec with Matchers {

  private val day6 = Day6()

  "parseTime" should {

    "parse time" in {
      val time = "Time:      7  15   30"
      day6.parseTime(time) shouldBe List(7, 15, 30)
    }

  }

  "parseDistance" should {

    "parse distance" in {
      val distance = "Distance:  9  40  200"
      day6.parseDistance(distance) shouldBe List(9, 40, 200)
    }
  }

  "getDistance" when {

    "return millimetres for the given milliseconds" in {
      day6.getDistance(0, 7) shouldBe 0
      day6.getDistance(1, 7) shouldBe 6
      day6.getDistance(2, 7) shouldBe 10
      day6.getDistance(3, 7) shouldBe 12
      day6.getDistance(4, 7) shouldBe 12
      day6.getDistance(5, 7) shouldBe 10
      day6.getDistance(6, 7) shouldBe 6
      day6.getDistance(7, 7) shouldBe 0
    }

  }

  "howManyWaysCanWin" should {

    "return how many ways you can win" in {
      day6.howManyWaysCanWin(7, 9) shouldBe 4
      day6.howManyWaysCanWin(15, 40) shouldBe 8
      day6.howManyWaysCanWin(30, 200) shouldBe 9
    }

  }

  "processInput" should {

    "get result from input" in {
      val input = "Time:      7  15   30\nDistance:  9  40  200"
      day6.processInput(input) shouldBe 288
    }

  }

}
