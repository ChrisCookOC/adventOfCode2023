package aoc.day4

import scala.collection.immutable.SortedMap
import scala.io.Source

case class Day4() {
  def sumUpScratchCards(board: SortedMap[Int, Game]): Int = {
    board.map(x => x._2.count).sum
  }

  def updateCardCount(fullBoardGame: SortedMap[Int, Game], gameBeingPlayed: Game): SortedMap[Int, Game] = {

    val winCount = getYourWinners(gameBeingPlayed).length
    val range = Range.inclusive(gameBeingPlayed.id + 1, gameBeingPlayed.id + winCount)

    fullBoardGame.map {
      case x: (Int, Game) if range.contains(x._1) =>
        (x._1, x._2.copy(count = x._2.count + gameBeingPlayed.count))
      case y: (Int, Game) => y
    }

  }

  def processBoardAndGetTotals(fullBoard: String): (Int, Int) = {
    calculateTotals(parseWholeThing(fullBoard))
  }

  def calculateTotals(fullBoard: SortedMap[Int, Game]): (Int, Int) = {

    val newBoard = fullBoard.foldLeft(fullBoard)((board, game) => {
      board ++ updateCardCount(board, board(game._1))
    })

    println("Board printing")
    newBoard.toSeq.sortBy(_._1).foreach { x =>
      println(s"${x._2} and match count is ${getYourWinners(x._2).length}")
    }
    println("End printing")

    (fullBoard.map(game => calculatePoints(getYourWinners(game._2))).sum, sumUpScratchCards(newBoard))
  }

  def calculatePoints(winners: List[Int]): Int = Math.pow(2, winners.length - 1).toInt

  def getYourWinners(game: Game): List[Int] = {
    game.ourNos.filter(x => game.winningNos.contains(x)).sorted
  }

  def parseWholeThing(wholeThing: String): SortedMap[Int, Game] = {
    SortedMap(wholeThing.split("\n").map(x => parseLine(x)).map(x => x.id -> x): _*)
  }

  def parseLine(line: String): Game = {
    val regexResults = "Card +(\\d+): ([\\d ]*)\\|([\\d ]*)".r.findAllIn(line).matchData.toList.head
    val gameNo = regexResults.group(1).toInt
    val winners = regexResults.group(2).trim.split(" +").toList.map(_.toInt)
    val ourNumbers = regexResults.group(3).trim.split(" +").toList.map(_.toInt)
    Game(gameNo, winners, ourNumbers, 1)
  }

  def run(): Unit = {

    val file = Source.fromResource("day4Input.txt")

    val input = file.getLines().mkString("\n")

    val total = processBoardAndGetTotals(input)

    println(s"Sum of totals is ${total._1}")
    println(s"Scratchcard count is ${total._2}")

  }

}


case class Game(id: Int, winningNos: List[Int], ourNos: List[Int], count: Int)
