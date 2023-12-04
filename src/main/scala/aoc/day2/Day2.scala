package aoc.day2

import scala.io.Source

case class Day2() {
  def findSumOfPowersOfValidGames(input: String): Int = {
    val games = input.split("\n")

    games.foldLeft(0) { (count, gameStr) =>
      val game = createGameFromEntry(gameStr)

      count + calculatePowerOfGame(findMinNumOfCubes(game))

    }

  }

  def calculatePowerOfGame(set: (Int, Int, Int)):Int = {
    set._1 * set._2 * set._3
  }

  def findMinNumOfCubes(game: Game):(Int, Int, Int) = {
    game.games.foldLeft((0,0,0)) {
      (maxVals, game) =>

        val blue = Math.max(maxVals._1, game.blue)
        val red = Math.max(maxVals._2, game.red)
        val green = Math.max(maxVals._3, game.green)

        (blue, red, green)

    }
  }

  def findSumOfIdsOfValidGames(input: String): Int = {
    val games = input.split("\n")

    games.foldLeft(0) { (count, gameStr) =>
      val game = createGameFromEntry(gameStr)
      if (isGameValid(game)) {
        count + game.id
      } else {
        count
      }
    }

  }

  def isGameValid(game: Game): Boolean = {

    val maxRed = 12
    val maxGreen = 13
    val maxBlue = 14

    game.games.foldLeft(true)((curr, game) => curr && game.blue <= maxBlue && game.red <= maxRed && game.green <= maxGreen)

  }


  def createGameFromEntry(entry: String): Game = {
    val gameNo = "(?<=Game )\\d+".r.findFirstIn(entry).getOrElse("0")

    val gamesSection = entry.split(":").last
    val games = gamesSection.split(";")

    val gameList: List[IndividualGame] = games.map { game: String =>

      val red: String = "\\d+(?= red)".r.findFirstIn(game).getOrElse("0")
      val green: String = "\\d+(?= green)".r.findFirstIn(game).getOrElse("0")
      val blue: String = "\\d+(?= blue)".r.findFirstIn(game).getOrElse("0")

      IndividualGame(red = red.toInt, green = green.toInt, blue = blue.toInt)

    }.toList

    Game(gameNo.toInt, gameList)
  }

  def run(): Unit = {

    val file = getClass.getResourceAsStream("input.txt")

    val input = Source.fromInputStream(file).mkString

    val resultValid = findSumOfIdsOfValidGames(input)
    val resultPowers = findSumOfPowersOfValidGames(input)

    println(s"Sum of valid game IDs is $resultValid")
    println(s"Sum of powers is $resultPowers")

  }


}

case class Game(id: Int, games: List[IndividualGame])

case class IndividualGame(blue: Int, red: Int, green: Int)