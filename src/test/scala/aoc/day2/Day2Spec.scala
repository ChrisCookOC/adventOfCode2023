package aoc.day2

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day2Spec extends AnyWordSpec with Matchers {

  private val day2 = Day2()

  "createGameFromEntry" should {

    "create game from entry" in {

      val input1 = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"

      day2.createGameFromEntry(input1) shouldBe Game(1, List(IndividualGame(3, 4, 0), IndividualGame(6, 1, 2), IndividualGame(0, 0, 2)))

      val input2 = "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

      day2.createGameFromEntry(input2) shouldBe Game(5, List(IndividualGame(1, 6, 3), IndividualGame(2, 1, 2)))

      val input3 = "Game 32: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"

      day2.createGameFromEntry(input3) shouldBe Game(32, List(IndividualGame(6, 20, 8), IndividualGame(5, 4, 13), IndividualGame(0, 1, 5)))

    }

  }

  "isGameValid" should {

    "mark game as valid if less pulled out than max" in {
      val game = Game(1, List(IndividualGame(3, 4, 0), IndividualGame(6, 1, 2), IndividualGame(0, 0, 2)))

      day2.isGameValid(game) shouldBe true

    }

    "mark game as valid if equal to the max" in {
      val game = Game(1, List(IndividualGame(14, 12, 13), IndividualGame(6, 1, 2), IndividualGame(0, 0, 2)))

      day2.isGameValid(game) shouldBe true

    }

    "mark game as invalid if exceeds the max" in {
      val game = Game(1, List(IndividualGame(14, 12, 13), IndividualGame(6, 1, 2), IndividualGame(15, 14, 13)))

      day2.isGameValid(game) shouldBe false

    }

  }

  "findSumOfIdsOfValidGames" should {

    "sum ids" in {

      val input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

      day2.findSumOfIdsOfValidGames(input) shouldBe 8

    }

  }

  "findMinNumOfCubes" should {

    "find minimum number of cubes for given game" in {

      val game = Game(1, List(IndividualGame(3, 4, 0), IndividualGame(6, 1, 2), IndividualGame(0, 0, 2)))

      day2.findMinNumOfCubes(game) shouldBe(6, 4, 2)

    }

  }

  "calculatePowerOfGame" should {

    "multiply the minimum number of cubes for each colour together" in {

      day2.calculatePowerOfGame((6, 4, 2)) shouldBe 48

    }

  }

  "findSumOfPowersOfValidGames" should {

    "sum powers" in {

      val input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"

      day2.findSumOfPowersOfValidGames(input) shouldBe 2286

    }

  }


}
