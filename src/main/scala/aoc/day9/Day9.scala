package aoc.day9

import scala.annotation.tailrec
import scala.io.Source

case class Day9() {
  def sumAllSeqs(entries: String): Int = {

    val lines = entries.split("\n")

    lines.map(x => findNextNumber(findSequenceRules(x))).sum

  }

  def findNextNumber(rules: List[Seq[Int]]): Int = {

    rules.reverse.foldLeft(0) { (sum, list) =>
      sum + list.last
    }

  }

  def parseSequence(str: String): Seq[Int] = str.split(" +").toSeq.map(_.toInt)

  def findDifferenceSequence(seq: Seq[Int]): Seq[Int] = {
    seq.sliding(2).foldLeft(Seq.empty[Int]) { (cur, pair) =>
      cur :+ (pair.last - pair.head)
    }
  }

  def findSequenceRules(str: String): List[Seq[Int]] = {

    val initial = parseSequence(str)

    getNextSequenceFromCurrentSequence(List(initial))

  }

  @tailrec
  private def getNextSequenceFromCurrentSequence(seq: List[Seq[Int]]): List[Seq[Int]] = {

    if (seq.last.count(x => x == 0) == seq.last.length) {
      seq
    } else {
      val newEntry = seq :+ findDifferenceSequence(seq.last)
      getNextSequenceFromCurrentSequence(newEntry)
    }

  }

  def run(): Unit = {

    val file = Source.fromResource("day9Input.txt").getLines().mkString("\n")

    val sum = sumAllSeqs(file)

    println(s"Sum of next entries is $sum")

  }

}
