package aoc.day10

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day10Spec extends AnyWordSpec with Matchers {

  private val day10 = Day10()

  "findStepsFurthestPoint" should {

    "solve the thing" in {
      val loop1 = ".....\n.S-7.\n.|.|.\n.L-J.\n....."
      day10.findStepsFurthestPoint(loop1) shouldBe 4

      val loop2 = "-L|F7\n7S-7|\nL|7||\n-L-J|\nL|-JF"
      day10.findStepsFurthestPoint(loop2) shouldBe 4

      val loop3 = "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..."
      day10.findStepsFurthestPoint(loop3) shouldBe 8

      val loop4 = "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ"
      day10.findStepsFurthestPoint(loop4) shouldBe 8

    }

  }

  "findNextCellInLoop" should {
    //Return itself if no match

    "find next when S" when {

      "dash" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.S-\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.S.\n.-.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n-S.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".-.\n.S.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

      }

      "pipe" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.S|\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.S.\n.|.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n|S.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".|.\n.S.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 0)

        }

      }

      "L" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.SL\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.S.\n.L.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nLS.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".L.\n.S.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

      }

      "J" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.SJ\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.S.\n.J.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nJS.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".J.\n.S.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

      }

      "7" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.S7\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.S.\n.7.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n7S.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".7.\n.S.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 0)

        }

      }

      "F" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.SF\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.S.\n.F.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nFS.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".F.\n.S.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "S", List((1, 1))) shouldBe(1, 0)

        }

      }

    }

    "find next when -" when {

      "dash" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.--\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.-.\n.-.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n--.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".-.\n.-.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

      }

      "pipe" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.-|\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.-.\n.|.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n|-.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".|.\n.-.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

      }

      "L" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.-L\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.-.\n.L.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nL-.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".L.\n.-.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

      }

      "J" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.-J\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.-.\n.J.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nJ-.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".J.\n.-.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

      }

      "7" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.-7\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.-.\n.7.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n7-.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".7.\n.-.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

      }

      "F" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.-F\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.-.\n.F.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nF-.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".F.\n.-.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "-", List((1, 1))) shouldBe(1, 1)

        }

      }

    }

    "find next when |" when {

      "dash" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.|-\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.|.\n.-.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n-|.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".-.\n.|.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

      }

      "pipe" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.||\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.|.\n.|.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n||.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".|.\n.|.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 0)

        }

      }

      "L" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.|L\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.|.\n.L.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nL|.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".L.\n.|.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

      }

      "J" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.|J\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.|.\n.J.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nJ|.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".J.\n.|.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

      }

      "7" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.|7\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.|.\n.7.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n7|.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".7.\n.|.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 0)

        }

      }

      "F" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.|F\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.|.\n.F.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nF|.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".F.\n.|.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "|", List((1, 1))) shouldBe(1, 0)

        }

      }

    }

    "find next when L" when {

      "dash" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.L-\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.L.\n.-.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n-L.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".-.\n.L.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

      }

      "pipe" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.L|\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.L.\n.|.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n|L.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".|.\n.L.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 0)

        }

      }

      "L" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.LL\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.L.\n.L.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nLL.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".L.\n.L.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

      }

      "J" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.LJ\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.L.\n.J.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nJL.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".J.\n.L.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

      }

      "7" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.L7\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.L.\n.7.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n7L.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".7.\n.L.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 0)

        }

      }

      "F" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.LF\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.L.\n.F.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nFL.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".F.\n.L.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "L", List((1, 1))) shouldBe(1, 0)

        }

      }

    }

    "find next when J" when {

      "dash" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.J-\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.J.\n.-.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n-J.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".-.\n.J.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

      }

      "pipe" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.J|\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.J.\n.|.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n|J.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".|.\n.J.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 0)

        }

      }

      "L" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.JL\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.J.\n.L.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nLJ.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".J.\n.J.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

      }

      "J" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.JJ\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.J.\n.J.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nJJ.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".J.\n.J.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

      }

      "7" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.J7\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.J.\n.7.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n7J.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".7.\n.J.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 0)

        }

      }

      "F" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.JF\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.J.\n.F.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nFJ.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".F.\n.J.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "J", List((1, 1))) shouldBe(1, 0)

        }

      }

    }

    "find next when 7" when {

      "dash" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.7-\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.7.\n.-.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n-7.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".-.\n.7.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

      }

      "pipe" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.7|\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.7.\n.|.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n|7.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".|.\n.7.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

      }

      "L" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.7L\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.7.\n.L.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nL7.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".L.\n.7.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

      }

      "7" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.77\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.7.\n.7.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n77.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".7.\n.7.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

      }

      "J" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.7J\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.7.\n.J.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nJ7.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".J.\n.7.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

      }

      "F" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.7F\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.7.\n.F.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nF7.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(0, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".F.\n.7.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "7", List((1, 1))) shouldBe(1, 1)

        }

      }

    }

    "find next when F" when {

      "dash" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.F-\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.F.\n.-.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n-F.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".-.\n.F.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

      }

      "pipe" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.F|\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.F.\n.|.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n|F.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".|.\n.F.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

      }

      "L" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.FL\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.F.\n.L.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nLF.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".L.\n.F.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

      }

      "F" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.FF\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.F.\n.F.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nFF.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".F.\n.F.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

      }

      "J" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.FJ\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.F.\n.J.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 2)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\nJF.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".J.\n.F.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

      }

      "7" when {

        "right" in {

          val smallGrid = day10.createGrid("...\n.F7\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(2, 1)

        }

        "below" in {

          val smallGrid = day10.createGrid("...\n.F.\n.7.")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "left" in {

          val smallGrid = day10.createGrid("...\n7F.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

        "above" in {

          val smallGrid = day10.createGrid(".7.\n.F.\n...")

          day10.findNextCellInLoop(smallGrid, (1, 1), "F", List((1, 1))) shouldBe(1, 1)

        }

      }

    }

  }

  "findMainLoopSize" should {

    "find loop when nothing else happening" in {
      val loop = ".....\n.S-7.\n.|.|.\n.L-J.\n....."
      day10.findMainLoopSize(loop) shouldBe 8
    }

  }

  "isPipe" should {

    "return true when pipe tile" in {
      day10.isPipe("|") shouldBe true
      day10.isPipe("-") shouldBe true
      day10.isPipe("J") shouldBe true
      day10.isPipe("L") shouldBe true
      day10.isPipe("7") shouldBe true
      day10.isPipe("F") shouldBe true
      day10.isPipe("S") shouldBe true
    }

    "return false when ground" in {
      day10.isPipe(".") shouldBe false
    }

  }

  "parseGrid" should {

    "create a grid from an entry" in {

      val entry = ".....\n.S-7.\n.|.|.\n.L-J.\n....."

      day10.createGrid(entry) shouldBe Map(
        (0, 0) -> ".",
        (1, 0) -> ".",
        (2, 0) -> ".",
        (3, 0) -> ".",
        (4, 0) -> ".",
        (0, 1) -> ".",
        (1, 1) -> "S",
        (2, 1) -> "-",
        (3, 1) -> "7",
        (4, 1) -> ".",
        (0, 2) -> ".",
        (1, 2) -> "|",
        (2, 2) -> ".",
        (3, 2) -> "|",
        (4, 2) -> ".",
        (0, 3) -> ".",
        (1, 3) -> "L",
        (2, 3) -> "-",
        (3, 3) -> "J",
        (4, 3) -> ".",
        (0, 4) -> ".",
        (1, 4) -> ".",
        (2, 4) -> ".",
        (3, 4) -> ".",
        (4, 4) -> "."
      )

    }

  }
}
