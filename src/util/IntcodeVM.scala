package util

import scala.annotation.tailrec

class IntcodeVM(program: Array[Int]) {

  def run(input: List[Int]): Int = runH(program, 0, input)

  @tailrec
  private def runH(program: Array[Int], opInd: Int, input: List[Int]): Int = {
    val (operation, p1, p2, p3) = load(program, opInd)
    operation match {
      case 1 =>
        val res = p1 + p2
        runH(program.updated(program(opInd + 3), res), opInd + 4, input)
      case 2 =>
        val res = p1 * p2
        runH(program.updated(program(opInd + 3), res), opInd + 4, input)
      case 3 =>
        runH(program.updated(program(opInd + 1), input.head), opInd + 2, input.tail)
      case 4 =>
        println(p1)
        runH(program, opInd + 2, input)
      case 5 =>
        if(p1 != 0) {
          runH(program, p2, input)
        } else {
          runH(program, opInd + 3, input)
        }
      case 6 =>
        if(p1 == 0) {
          runH(program, p2, input)
        } else {
          runH(program, opInd + 3, input)
        }
      case 7 =>
          runH(program.updated(p3, boolToInt(p1 < p2)), opInd + 4, input)
      case 8 =>
          runH(program.updated(p3, boolToInt(p1 == p2)), opInd + 4, input)
      case 99 =>
        program(0)
    }
  }

  private def load(program: Array[Int], opInd: Int) = {
    val opcode = program(opInd)
    val operation = opcode % 100
    val first = (opcode / 100) % 10
    val second = (opcode / 1000) % 10
    val third = (opcode / 10000) % 10

    val p1 = if (first == 1 || isOutput(operation, 1)) get(program, opInd + 1) else get(program, get(program, opInd + 1))
    val p2 = if (second == 1 || isOutput(operation, 2)) get(program, opInd + 2) else get(program, get(program, opInd + 2))
    val p3 = if(third == 1 || isOutput(operation, 3)) get(program, opInd +3) else get(program, get(program, opInd + 3))

    (operation, p1, p2, p3)
  }

  private def get(program: Array[Int], pos: Int): Int = program.lift(pos).getOrElse(0)

  private def isOutput(opcode: Int, position: Int): Boolean = {
    opcode match {
      case 1 | 2 | 7 | 8 => position == 3
      case 3 => position == 2
      case _ => false
    }
  }

  private def boolToInt(b: Boolean): Int = if (b) 1 else 0
}