package aoc.day8

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.io.Source

case class Day8() {
  def runJourneyAllAtOnce(instructions: Instructions): Long = {

    val instructionItems = instructions.instructions.split("")

    val startPoints = instructions.nodes
      .filter(x => x._1.endsWith("A"))
      .map(x => x._2.name)
      .toArray

    val eachCycle = startPoints.map(x => doStuff(instructions, instructionItems, x, 0))
    multiLcm(eachCycle)

  }

  def runJourney(instructions: Instructions): Long = {

    val instructionItems = instructions.instructions.split("")

    doStuff(instructions, instructionItems, "AAA", 0)

  }

  @tailrec
  private def doStuff(instructions: Instructions, instructionItems: Array[String],
                      startPlace: String, startCount: Long): Long = {

    val position = instructionItems.foldLeft((startPlace, startCount)) {
      (cur, instr) =>
        if (!cur._1.endsWith("Z")) (applyMove(instr, instructions, cur._1), cur._2 + 1)
        else cur
    }

    if (position._1.endsWith("Z")) {
      position._2
    } else {
      doStuff(instructions, instructionItems, position._1, position._2)
    }

  }

  def applyMove(leftOrRight: String, instructions: Instructions, currentPlace: String): String = {
    val currentNode = instructions.nodes(currentPlace)
    if (leftOrRight == "R") {
      currentNode.right
    } else {
      currentNode.left
    }
  }

  def parseInput(input: String): Instructions = {

    val lines = input.split("\n+")

    Instructions(lines.head, SortedMap(lines.tail.map(x => parseLine(x)).map(x => x.name -> x): _*))

  }

  def parseLine(str: String): Node = {
    val words = str.split("[= (,)]+")
    Node(words.head, words(1), words(2))
  }

  def run(): Unit = {

    val file = Source.fromResource("day8Input.txt").getLines().mkString("\n")

    val moves = runJourneyAllAtOnce(parseInput(file))

    println(s"Moves are $moves")

  }

  //Euclid algorithim from wikipedia
  @tailrec
  final def gcd(a: Long, b: Long): Long = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  def lcm(a: Long, b: Long): Long = (a * b) / gcd(a, b)

  def multiLcm(numbers: Seq[Long]): Long = {
    numbers.foldLeft(1L) { (cur, value) => {
      lcm(cur, value)
    }
    }
  }
}

case class Node(name: String, left: String, right: String)

case class Instructions(instructions: String, nodes: Map[String, Node])