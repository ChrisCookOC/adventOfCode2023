package aoc.day9

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day9Spec extends AnyWordSpec with Matchers {

  private val day9 = Day9()

  private val list1 = List(
    Seq(0, 3, 6, 9, 12, 15),
    Seq(3, 3, 3, 3, 3),
    Seq(0, 0, 0, 0)
  )

  private val list2 = List(
    Seq(1, 3, 6, 10, 15, 21),
    Seq(2, 3, 4, 5, 6),
    Seq(1, 1, 1, 1),
    Seq(0, 0, 0)
  )

  "parseSequence" should {

    "split sequence into entries" in {

      day9.parseSequence("0 3 6 9 12 15") shouldBe Seq(0, 3, 6, 9, 12, 15)

    }

  }

  "findDifferenceSequence" should {

    "find sequence of numbers that are the difference" in {

      day9.findDifferenceSequence(Seq(0, 3, 6, 9, 12, 15)) shouldBe Seq(3, 3, 3, 3, 3)
      day9.findDifferenceSequence(Seq(3, 3, 3, 3, 3)) shouldBe Seq(0, 0, 0, 0)

    }

  }

  "findSequenceRules" should {

    "find the difference between the item in the supplied string" in {

      day9.findSequenceRules("0 3 6 9 12 15") shouldBe list1

      day9.findSequenceRules("1   3   6  10  15  21") shouldBe list2

    }

  }

  "findNextNumber" should {

    "find next number in the sequence" in {

      day9.findNextNumber(list1) shouldBe 18
      day9.findNextNumber(list2) shouldBe 28

    }

  }

  "findPrevNumber" should {

    "find previous number in the sequence" in {

      day9.findPrevNumber(list1) shouldBe -3
      day9.findPrevNumber(list2) shouldBe 0

    }

  }

  "addPrevNumberToSeq" should {

    "return list with previous number at the start" in {
      day9.addPrevNumberToSeq(Seq(0,0,0), Seq(2,2,2,2)) shouldBe Seq(2,2,2,2,2)
      day9.addPrevNumberToSeq(Seq(2,2,2,2,2), Seq(0,2,4,6,8)) shouldBe Seq(-2, 0, 2, 4, 6, 8)
      day9.addPrevNumberToSeq(Seq(-2,0,2,4,6,8), Seq(3,3,5,9,15,23)) shouldBe Seq(5,3,3,5,9,15,23)
    }

  }

  "sumAllSeqs" should {

    "sum up all the next and prev entries in the sequence" in {
      day9.sumAllSeqs("0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45") shouldBe (114,2)
    }

  }

}
