package aoc

import aoc.day1.Day1
object App {

  def main(args: Array[String]): Unit = {

    val whichDayToRun = {
      if (args.length > 0) {
        args.head
      } else {
        0
      }
    }

    whichDayToRun match {
      case _ =>     Day1().run()

    }
  }

}
