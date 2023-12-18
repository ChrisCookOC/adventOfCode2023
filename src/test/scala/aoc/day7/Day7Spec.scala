package aoc.day7

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day7Spec extends AnyWordSpec with Matchers {

  private val day7 = Day7()

  "parseLine" should {

    "turn line into object" in {
      day7.parseLine("32J3K 765") shouldBe Hand("32J3K", 765, HandTypes.ThreeOfAKind)
    }

  }

  "findHandResult" should {

    "do that" in {
      day7.findHandResult("KKKKK") shouldBe HandTypes.FiveOfAKind
      day7.findHandResult("KKKKQ") shouldBe HandTypes.FourOfAKind
      day7.findHandResult("KKQQQ") shouldBe HandTypes.FullHouse
      day7.findHandResult("Q3Q2Q") shouldBe HandTypes.ThreeOfAKind
      day7.findHandResult("Q5Q53") shouldBe HandTypes.TwoPair
      day7.findHandResult("Q4834") shouldBe HandTypes.OnePair
      day7.findHandResult("32564") shouldBe HandTypes.HighCard
    }

    "use J as a Joker wild card" in {
      day7.findHandResult("KKKKJ") shouldBe HandTypes.FiveOfAKind
      day7.findHandResult("KKJKJ") shouldBe HandTypes.FiveOfAKind
      day7.findHandResult("KJJJK") shouldBe HandTypes.FiveOfAKind
      day7.findHandResult("JJJKJ") shouldBe HandTypes.FiveOfAKind

      day7.findHandResult("KKKJQ") shouldBe HandTypes.FourOfAKind
      day7.findHandResult("KJJKQ") shouldBe HandTypes.FourOfAKind
      day7.findHandResult("JJJKQ") shouldBe HandTypes.FourOfAKind

      day7.findHandResult("KKQQJ") shouldBe HandTypes.FullHouse

      day7.findHandResult("Q3J2Q") shouldBe HandTypes.ThreeOfAKind
      day7.findHandResult("57J2J") shouldBe HandTypes.ThreeOfAKind

      day7.findHandResult("Q2J34") shouldBe HandTypes.OnePair

    }

  }

  "isHandTwoHigherThanHandOne" should {

    "compare card wise if same value" in {

      day7.isHandTwoHigherThanHandOne(
        Hand("33332", 0, HandTypes.FourOfAKind),
        Hand("2AAAA", 0, HandTypes.FourOfAKind)
      ) shouldBe false

      day7.isHandTwoHigherThanHandOne(
        Hand("2AAAA", 0, HandTypes.FourOfAKind),
        Hand("33332", 0, HandTypes.FourOfAKind)
      ) shouldBe true

      day7.isHandTwoHigherThanHandOne(
        Hand("77788", 0, HandTypes.FullHouse),
        Hand("77888", 0, HandTypes.FullHouse)
      ) shouldBe true

      day7.isHandTwoHigherThanHandOne(
        Hand("77888", 0, HandTypes.FullHouse),
        Hand("77788", 0, HandTypes.FullHouse)
      ) shouldBe false

    }

    "use value if different values" in {

      day7.isHandTwoHigherThanHandOne(
        Hand("77777", 0, HandTypes.FiveOfAKind),
        Hand("77788", 0, HandTypes.FullHouse)
      ) shouldBe false

      day7.isHandTwoHigherThanHandOne(
        Hand("77A23", 0, HandTypes.OnePair),
        Hand("77788", 0, HandTypes.FullHouse)
      ) shouldBe true

    }

    "treat J for Joker as lowest value" in {
      day7.isHandTwoHigherThanHandOne(
        Hand("77888", 0, HandTypes.FullHouse),
        Hand("J7788", 0, HandTypes.FullHouse)
      ) shouldBe false
    }
  }

  "parseWholeList" should {

    "turn line into sorted list of objects" in {
      val list = "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483"
      day7.parseWholeList(list) shouldBe List(
        Hand("32T3K", 765, HandTypes.OnePair),
        Hand("KK677", 28, HandTypes.TwoPair),
        Hand("T55J5", 684, HandTypes.FourOfAKind),
        Hand("QQQJA", 483, HandTypes.FourOfAKind),
        Hand("KTJJT", 220, HandTypes.FourOfAKind)
      )
    }

  }

  "workOutPoints" should {

    "work out points" in {

      val hands = List(
        Hand("32T3K", 765, HandTypes.OnePair),
        Hand("KK677", 28, HandTypes.TwoPair),
        Hand("T55J5", 684, HandTypes.FourOfAKind),
        Hand("QQQJA", 483, HandTypes.FourOfAKind),
        Hand("KTJJT", 220, HandTypes.FourOfAKind)
      )

      day7.workOutPoints(hands) shouldBe 5905

    }

  }

}
