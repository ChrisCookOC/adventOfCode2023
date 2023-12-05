package aoc

import aoc.day1.Day1
import aoc.day2.Day2
import aoc.day3.Day3

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
      case "1" => Day1().run()
      case "2" => Day2().run()
      case _ => Day3().run()
    }
  }

}
