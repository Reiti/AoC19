import util.Util

import scala.annotation.tailrec

object Day2 {
  def main(args: Array[String]): Unit = {
    val program: Array[Int] = Util.loadDay(2).split(",") map {_.toInt}

    //Part 1
    println(run(program.updated(1, 12)))

    //Part 2
    val perms = for {
      noun <- (0 to 99).view
      verb <- (0 to 99).view
    } yield (noun, verb)

    val res = perms find {case (noun, verb) => run(program.updated(1, noun).updated(2, verb)) == 19690720}

    res match {
      case Some((noun, verb)) => println(noun * 100 + verb)
      case None => println("no solution found")
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
