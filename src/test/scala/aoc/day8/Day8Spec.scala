package aoc.day8

import aoc.day7.Day7
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.SortedMap


class Day8Spec extends AnyWordSpec with Matchers {

  private val day8 = Day8()

  private val instructions = Instructions("RL", SortedMap(
    "AAA" -> Node("AAA", "BBB", "CCC"),
    "BBB" -> Node("BBB", "DDD", "EEE"),
    "CCC" -> Node("CCC", "ZZZ", "GGG"),
    "DDD" -> Node("DDD", "DDD", "DDD"),
    "EEE" -> Node("EEE", "EEE", "EEE"),
    "GGG" -> Node("GGG", "GGG", "GGG"),
    "ZZZ" -> Node("ZZZ", "ZZZ", "ZZZ")
  ))

  private val instructions2 = Instructions("LLR", SortedMap(
    "AAA" -> Node("AAA", "BBB", "BBB"),
    "BBB" -> Node("BBB", "AAA", "ZZZ"),
    "ZZZ" -> Node("ZZZ", "ZZZ", "ZZZ")
  ))

  private val instructions3 = Instructions("LR", SortedMap(
    "11A" -> Node("11A", "11B", "XXX"),
    "11B" -> Node("11B", "XXX", "11Z"),
  "11Z" -> Node("11Z", "11B", "XXX"),
  "22A" -> Node("22A", "22B", "XXX"),
  "22B" -> Node("22B", "22C", "22C"),
  "22C" -> Node("22C", "22Z", "22Z"),
  "22Z" -> Node("22Z", "22B", "22B"),
  "XXX" -> Node("XXX", "XXX", "XXX")
  ))

  "parseLine" should {

    "parseLine" in {
      day8.parseLine("AAA = (BBB, CCC)") shouldBe Node("AAA", "BBB", "CCC")
    }

  }

  "parseInput" should {

    "create instruction set" in {
      val input = "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)" +
        "\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)"

      day8.parseInput(input) shouldBe instructions

    }

  }

  "applyMove" should {

    "go from one to the next" in {

      day8.applyMove("R", instructions, "AAA") shouldBe "CCC"
      day8.applyMove("L", instructions, "AAA") shouldBe "BBB"

    }

  }

  "runJourney" should {

    "find how many moves to get to ZZZ" in {
      day8.runJourney(instructions) shouldBe 2
      day8.runJourney(instructions2) shouldBe 6
    }

  }

  "runJourneyAllAtOnce" should {

    "find how many moves to get to ending with Z in the new way" in {
      day8.runJourneyAllAtOnce(instructions3) shouldBe 6
    }

  }

  "gcd" should {

    "do the maths" in {
      day8.gcd(1071, 462) shouldBe 21
    }

  }

  "lcm" should {

    "do the maths" in {
      day8.lcm(21, 6) shouldBe 42
      day8.lcm(21, 73) shouldBe 1533
      day8.lcm(42, 73) shouldBe 3066
    }

  }

  "multiple lcm" should {

    "find lcm of multiple numbers" in {

      day8.multiLcm(Seq(42,73,89,3,54)) shouldBe 2455866
    }
  }

}
