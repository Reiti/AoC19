package util

import scala.annotation.tailrec

class IntcodeVM(program: Map[Int, BigInt]) {

  def run(input: Pipe, output: Pipe): Pipe = runH(program, 0, input, output, 0)

  @tailrec
  private def runH(program: Map[Int, BigInt], opInd: Int, input: Pipe, output: Pipe, relOff: Int): Pipe = {
    val (operation, p1, p2, p3) = load(program, opInd, relOff)
    operation match {
      case 1 =>
        val res = p1 + p2
        runH(program.updated(p3.toInt, res), opInd + 4, input, output, relOff)
      case 2 =>
        val res = p1 * p2
        runH(program.updated(p3.toInt, res), opInd + 4, input, output, relOff)
      case 3 =>
        runH(program.updated(p1.toInt, input.read()), opInd + 2, input, output, relOff)
      case 4 =>
        output.write(p1)
        //println(p1)
        runH(program, opInd + 2, input, output, relOff)
      case 5 =>
        val target = if (p1 != 0) p2.toInt else opInd + 3
        runH(program, target, input, output, relOff)
      case 6 =>
        val target = if (p1 == 0) p2.toInt else opInd + 3
        runH(program, target, input, output, relOff)
      case 7 =>
        runH(program.updated(p3.toInt, boolToInt(p1 < p2)), opInd + 4, input, output, relOff)
      case 8 =>
        runH(program.updated(p3.toInt, boolToInt(p1 == p2)), opInd + 4, input, output, relOff)
      case 9 =>
        runH(program, opInd + 2, input, output, relOff + p1.toInt)
      case 99 =>
        output
    }
  }

  private def load(program: Map[Int, BigInt], opInd: Int, relOff: Int) = {
    val opcode = program(opInd)
    val operation = (opcode % 100).toInt
    val first = ((opcode / 100) % 10).toInt
    val second = ((opcode / 1000) % 10).toInt
    val third = ((opcode / 10000) % 10).toInt

    val p1 = if (isOutput(operation, 1))  {
      if(first == 0)
        getI(program, opInd + 1)
      else
        getI(program, opInd + 1) + relOff
    }
    else {
      first match {
        case 0 => getP(program, opInd + 1)
        case 1 => getI(program, opInd + 1)
        case 2 => getR(program, opInd + 1, relOff)
      }
    }
    val p2 = if (isOutput(operation, 2)) {
      if(second == 0)
        getI(program, opInd + 2)
      else
        getI(program, opInd + 2) + relOff
    }
     else {
      second match {
        case 0 => getP(program, opInd + 2)
        case 1 => getI(program, opInd + 2)
        case 2 => getR(program, opInd + 2, relOff)
      }
    }
    val p3 = if(third == 1 || isOutput(operation, 3)) {
      if(third == 0)
        getI(program, opInd +3)
      else
        getI(program, opInd +3) + relOff
    } else {
      third match {
        case 0 => getP(program, opInd +3)
        case 1 => getI(program, opInd + 3)
        case 2 => getR(program, opInd + 3, relOff)
      }
    }

    (operation, p1, p2, p3)
  }



  private def getI(program: Map[Int, BigInt], pos: Int): BigInt = program.getOrElse(pos, BigInt(0))

  private def getP(program: Map[Int, BigInt], pos: Int): BigInt = getI(program, getI(program, pos).toInt)

  private def getR(program: Map[Int, BigInt], pos: Int, relOff: Int): BigInt = getI(program, (getI(program, pos) + relOff).toInt)

  private def isOutput(opcode: BigInt, position: Int): Boolean = {
    opcode.toInt match {
      case 1 | 2 | 7 | 8 => position == 3
      case 3 => position == 1
      case _ => false
    }
  }

  private def boolToInt(b: Boolean): Int = if (b) 1 else 0
}
