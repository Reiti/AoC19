package util

import scala.annotation.tailrec

class IntcodeProgram(program: Array[Int]) {

  def run(input: List[Int]): Int = runH(program, 0, input)

  @tailrec
  private def runH(program: Array[Int], opInd: Int, input: List[Int]): Int = {
    val (operation, first, second, third) = parse(program(opInd))
    operation match {
      case 1 =>
        val res = getValue(program, opInd + 1, first) + getValue(program, opInd + 2, second)
        runH(program.updated(program(opInd + 3), res), opInd + 4, input)
      case 2 =>
        val res = getValue(program, opInd + 1, first) * getValue(program, opInd + 2, second)
        runH(program.updated(program(opInd + 3), res), opInd + 4, input)
      case 3 =>
        val res = input.head
        runH(program.updated(program(opInd + 1), res), opInd + 2, input.tail)
      case 4 =>
        val res = getValue(program, opInd + 1, first)
        println(res)
        runH(program, opInd + 2, input)
      case 5 =>
        val test = getValue(program, opInd + 1, first)
        val target = getValue(program, opInd + 2, second)
        if(test != 0) {
          runH(program, target, input)
        } else {
          runH(program, opInd + 3, input)
        }
      case 6 =>
        val test = getValue(program, opInd + 1, first)
        val target = getValue(program, opInd + 2, second)
        if(test == 0) {
          runH(program, target, input)
        } else {
          runH(program, opInd + 3, input)
        }
      case 7 =>
        val op1 = getValue(program, opInd + 1, first)
        val op2 = getValue(program, opInd + 2, second)
        val target = program(opInd + 3)
        if(op1 < op2) {
          runH(program.updated(target, 1), opInd + 4, input)
        } else {
          runH(program.updated(target, 0), opInd + 4, input)
        }
      case 8 =>
        val op1 = getValue(program, opInd + 1, first)
        val op2 = getValue(program, opInd + 2, second)
        val target = program(opInd + 3)
        if(op1 == op2) {
          runH(program.updated(target, 1), opInd + 4, input)
        } else {
          runH(program.updated(target, 0), opInd + 4, input)
        }
      case 99 =>
        program(0)
    }
  }

  private def parse(opcode: Int) = {
    val operation = opcode % 100
    val first = (opcode / 100) % 10
    val second = (opcode / 1000) % 10
    val third = (opcode / 10000) % 10

    (operation, first, second, third)
  }

  private def getValue(program: Array[Int], pos: Int, mode: Int): Int = {
    if(mode == 1) {
      program(pos)
    }
    else {
      program(program(pos))
    }
  }


}
