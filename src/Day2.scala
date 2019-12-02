import util.Util

import scala.annotation.tailrec
import scala.util.control.Breaks._

object Day2 {
  def main(args: Array[String]): Unit = {
    val program: Array[Int] = Util.loadDay(2).split(",") map {_.toInt}

    //Part 1
    println(run(program.updated(1, 12)))

    //Part 2
    breakable {
      for (noun <- 0 to 99) {
        for (verb <- 0 to 99) {
          val alteredProgram = program.updated(1, noun).updated(2, verb)
          
          if (run(alteredProgram) == 19690720) {
            println(100 * noun + verb)
            break
          }
        }
      }
    }
  }

  def run(program: Array[Int]): Int = {
    @tailrec
    def runH(program: Array[Int], opInd: Int): Int = {
      if(program(opInd) == 99) {
        program(0)
      }
      else {
        val posA = program(opInd + 1)
        val posB = program(opInd + 2)
        val target = program(opInd + 3)

        val res = program(opInd) match {
          case 1 => program(posA) + program(posB)
          case 2 => program(posA) * program(posB)
        }

        runH(program.updated(target, res), opInd + 4)
      }
    }

    runH(program, 0)
  }
}
