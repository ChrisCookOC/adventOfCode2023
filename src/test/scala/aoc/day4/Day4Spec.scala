package aoc.day4

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.SortedMap

class Day4Spec extends AnyWordSpec with Matchers {

  private val day4 = Day4()

  private val line1 = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
  private val line2 = "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
  private val line3 = "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
  private val line4 = "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
  private val line5 = "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
  private val line6 = "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  private val fullBoard = s"$line1\n$line2\n$line3\n$line4\n$line5\n$line6"

  private val game1 = Game(1, List(41, 48, 83, 86, 17), List(83, 86, 6, 31, 17, 9, 48, 53), 1)
  private val game2 = Game(2, List(13, 32, 20, 16, 61), List(61, 30, 68, 82, 17, 32, 24, 19), 1)
  private val game3 = Game(3, List(1, 21, 53, 59, 44), List(69, 82, 63, 72, 16, 21, 14, 1), 1)
  private val game4 = Game(4, List(41, 92, 73, 84, 69), List(59, 84, 76, 51, 58, 5, 54, 83), 1)
  private val game5 = Game(5, List(87, 83, 26, 28, 32), List(88, 30, 70, 12, 93, 22, 82, 36), 1)
  private val game6 = Game(6, List(31, 18, 13, 56, 72), List(74, 77, 10, 23, 35, 67, 36, 11), 1)
  private val fullBoardGame = SortedMap(1 -> game1, 2 -> game2, 3 -> game3, 4 -> game4, 5 -> game5, 6 -> game6)

  "parseLine" should {

    "create a Game object" in {
      day4.parseLine(line1) shouldBe game1
    }

    "create a Game object when line no is multi digit and multi space" in {
      day4.parseLine("Card      10: 41 48 83 86 17 | 83 86  6 31 17  9 48 53") shouldBe game1.copy(id = 10)
    }

  }

  "parseWholeThing" should {

    "create list of Game objects" in {

      day4.parseWholeThing(fullBoard) shouldBe fullBoardGame

    }

  }

  "getYourWinners" should {

    "return the winners from the game" in {

      day4.getYourWinners(game1) shouldBe List(17, 48, 83, 86)

    }

  }

  "calculatePoints" should {

    "calculate points based on how many matches" in {
      day4.calculatePoints(List(17)) shouldBe 1
      day4.calculatePoints(List(17, 12)) shouldBe 2
      day4.calculatePoints(List(17, 12, 18)) shouldBe 4
      day4.calculatePoints(List(17, 12, 18, 1)) shouldBe 8
    }

  }

  "calculateTotals" should {

    "sum all the cards" in {
      day4.calculateTotals(fullBoardGame) shouldBe(13, 30)
    }

  }

  "processBoardAndGetTotals" should {

    "parse the grid and sum all the cards" in {
      day4.processBoardAndGetTotals(fullBoard) shouldBe(13, 30)
    }

    "parse example 2" in {

      val game = "Card   1: 33 56 23 64 92 86 94  7 59 13 | 86 92 64 43 10 70 16 55 79 33 56  8  7 25 82 14 31 96 94 13 99 29 69 75 23\n" +
        "Card   2: 61 66 75  1 27 38 93 90 34 43 | 94 46 62 49 35 88 45 70 15 22 20 86 56 38 64 98 50  6 79 11 13 93 92 60 16\n" +
        "Card   3: 57  7 33 56 85  6 88 34 80  8 | 92 42  7 60 61 51 40  6 67 35  3 25 87  2 98 75 97 54 10 68 73 83  4 62 56\n" +
        "Card   4: 79 85 94 74 15 62 84 88 76 56 | 56  9 22 57  4 92 62 79 84 64 72 55 34 88 66 15 45 18 76 73 85 94  8 78 74\n" +
        "Card   5: 57 94 99 25 52 67 69 31 26 78 | 94 52 31 83 70 45 40 67 89 11 81 24 25 61 26 72 50 12 27 69 91 57 55 34 78\n" +
        "Card   6:  5 96  3 19 98 25 13 59 94  8 | 36 55 22 76 86 19 10  8 59  9 87 40  2 71 13 98 12 77  3 70  5 25 34 41 88"

      day4.processBoardAndGetTotals(game) shouldBe(1158, 57)

    }
  }

  "sumUpScratchCards" should {

    "add up the Game scores" in {

      val board = SortedMap(
        1 -> game1,
        2 -> game2.copy(count = 2),
        3 -> game3.copy(count = 2),
        4 -> game4.copy(count = 2),
        5 -> game5.copy(count = 2),
        6 -> game6
      )

      day4.sumUpScratchCards(board) shouldBe 10
    }

  }

  "updateCardCountsBasedOnWinners" should {

    "update the card counts based on the winning card number" in {

      val firstLineResult = day4.updateCardCount(fullBoardGame, game1)
      firstLineResult shouldBe SortedMap(
        1 -> game1,
        2 -> game2.copy(count = 2),
        3 -> game3.copy(count = 2),
        4 -> game4.copy(count = 2),
        5 -> game5.copy(count = 2),
        6 -> game6
      )

      day4.updateCardCount(firstLineResult, firstLineResult(2)) shouldBe SortedMap(
        1 -> firstLineResult(1),
        2 -> firstLineResult(2),
        3 -> firstLineResult(3).copy(count = 4),
        4 -> firstLineResult(4).copy(count = 4),
        5 -> firstLineResult(5),
        6 -> firstLineResult(6)
      )

    }

  }

}
