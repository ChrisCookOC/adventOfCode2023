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

}
