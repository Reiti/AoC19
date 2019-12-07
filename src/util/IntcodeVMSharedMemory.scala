package util

import scala.annotation.tailrec

class IntcodeVMSharedMemory(program: Array[Int]) {

  def run(input: SharedMem, output: SharedMem): SharedMem = runH(program, 0, input, output)

  @tailrec
  private def runH(program: Array[Int], opInd: Int, input: SharedMem, output: SharedMem): SharedMem = {
    val (operation, p1, p2, p3) = load(program, opInd)
    operation match {
      case 1 =>
        val res = p1 + p2
        runH(program.updated(p3, res), opInd + 4, input, output)
      case 2 =>
        val res = p1 * p2
        runH(program.updated(p3, res), opInd + 4, input, output)
      case 3 =>
        while(input.empty()) {Thread.sleep(1)}
        val x = input.read().get
        runH(program.updated(p1, x), opInd + 2, input, output)
      case 4 =>
        output.write(p1)
        runH(program, opInd + 2, input, output)
      case 5 =>
        val target = if (p1 != 0) p2 else opInd + 3
        runH(program, target, input, output)
      case 6 =>
        val target = if (p1 == 0) p2 else opInd + 3
        runH(program, target, input, output)
      case 7 =>
        runH(program.updated(p3, boolToInt(p1 < p2)), opInd + 4, input, output)
      case 8 =>
        runH(program.updated(p3, boolToInt(p1 == p2)), opInd + 4, input, output)
      case 99 =>
        output
    }
  }

  private def load(program: Array[Int], opInd: Int) = {
    val opcode = program(opInd)
    val operation = opcode % 100
    val first = (opcode / 100) % 10
    val second = (opcode / 1000) % 10
    val third = (opcode / 10000) % 10

    val p1 = if (first == 1 || isOutput(operation, 1)) getI(program, opInd + 1) else getP(program, opInd + 1)
    val p2 = if (second == 1 || isOutput(operation, 2)) getI(program, opInd + 2) else getP(program, opInd + 2)
    val p3 = if(third == 1 || isOutput(operation, 3)) getI(program, opInd +3) else getP(program, opInd +3)

    (operation, p1, p2, p3)
  }

  private def getI(program: Array[Int], pos: Int): Int = program.lift(pos).getOrElse(0)

  private def getP(program: Array[Int], pos: Int): Int = getI(program, getI(program, pos))

  private def isOutput(opcode: Int, position: Int): Boolean = {
    opcode match {
      case 1 | 2 | 7 | 8 => position == 3
      case 3 => position == 1
      case _ => false
    }
  }

  private def boolToInt(b: Boolean): Int = if (b) 1 else 0
}
