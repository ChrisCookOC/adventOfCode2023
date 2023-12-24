package aoc.day9

import scala.annotation.tailrec
import scala.io.Source

case class Day9() {
  def addPrevNumberToSeq(lastSeq: Seq[Int], nextSeq: Seq[Int]): Seq[Int] = {
    (nextSeq.head - lastSeq.head) +: nextSeq
  }

  def findPrevNumber(rules: List[Seq[Int]]): Int = {
    rules.reverse.foldLeft(Seq(rules.last)) { (cur, item) =>
      cur :+ Seq(addPrevNumberToSeq(cur.last, item), item).flatten
    }.last.head
  }

  def sumAllSeqs(entries: String): (Int, Int) = {

    val lines = entries.split("\n")

    lines.map { x =>
      val rules = findSequenceRules(x)
      (findNextNumber(rules),
        findPrevNumber(rules))
    }.foldLeft((0, 0)) { (cur, item) =>
      (cur._1 + item._1, cur._2 + item._2)
    }

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
