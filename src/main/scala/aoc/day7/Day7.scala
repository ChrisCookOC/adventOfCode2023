package aoc.day7

import scala.annotation.tailrec
import scala.io.Source

case class Day7() {
  def workOutPoints(hands: List[Hand]): Int = {
    hands.zipWithIndex.map(x => x._1.bid * (x._2 + 1)).sum
  }

  def parseWholeList(list: String): List[Hand] = {

    list.split("\n").map(x => parseLine(x))
      .sortInPlaceWith((x, y) => isHandTwoHigherThanHandOne(x, y))
      .toList

  }

  def isHandTwoHigherThanHandOne(hand1: Hand, hand2: Hand): Boolean = {

    if (hand1.handType == hand2.handType) {
      compareCards(hand1.cards, hand2.cards, 0)
    } else {
      hand1.handType < hand2.handType
    }
  }

  @tailrec
  private def compareCards(hand1Cards: String, hand2Cards: String, cardNo: Int): Boolean = {
    val hand1Card = hand1Cards(cardNo)
    val hand2Card = hand2Cards(cardNo)

    if (hand1Card != hand2Card) {
      if (hand1Card == 'A') {
        false
      } else if (hand2Card == 'A') {
        true
      } else if (hand1Card == 'K') {
        false
      } else if (hand2Card == 'K') {
        true
      } else if (hand1Card == 'Q') {
        false
      } else if (hand2Card == 'Q') {
        true
      } else if (hand1Card == 'J') {
        false
      } else if (hand2Card == 'J') {
        true
      } else if (hand1Card == 'T') {
        false
      } else if (hand2Card == 'T') {
        true
      } else {
        hand1Card.toInt < hand2Card.toInt
      }
    } else {
      compareCards(hand1Cards, hand2Cards, cardNo + 1)
    }
  }

  def findHandResult(hand: String): HandTypes.Value = {

    val counts = List(
      hand.count(x => x == 'A'),
      hand.count(x => x == 'J'),
      hand.count(x => x == 'Q'),
      hand.count(x => x == 'K'),
      hand.count(x => x == 'T'),
      hand.count(x => x == '2'),
      hand.count(x => x == '3'),
      hand.count(x => x == '4'),
      hand.count(x => x == '5'),
      hand.count(x => x == '6'),
      hand.count(x => x == '7'),
      hand.count(x => x == '8'),
      hand.count(x => x == '9')
    )


    if (counts.contains(5)) {
      HandTypes.FiveOfAKind
    } else if (counts.contains(4)) {
      HandTypes.FourOfAKind
    } else if (counts.contains(3) && counts.contains(2)) {
      HandTypes.FullHouse
    } else if (counts.contains(3)) {
      HandTypes.ThreeOfAKind
    } else if (counts.count(x => x == 2) == 2) {
      HandTypes.TwoPair
    } else if (counts.contains(2)) {
      HandTypes.OnePair
    } else {
      HandTypes.HighCard
    }

  }

  def parseLine(line: String): Hand = {

    val split = line.split(" ")

    Hand(split(0), split(1).toInt, findHandResult(split(0)))

  }

  def run(): Unit = {

    val file = Source.fromResource("day7Input.txt")

    val input = file.getLines().mkString("\n")

    val score = workOutPoints(parseWholeList(input))

    println(s"Score is $score")

  }
}

case class Hand(cards: String, bid: Int, handType: HandTypes.Value)

object HandTypes extends Enumeration {
  type handType = Value

  val HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind = Value

}