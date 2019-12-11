package util

import scala.annotation.tailrec

class IntcodeVMSuspendable(program: Map[Long, Long]) {

  def run(input: List[Long], output: List[Long]): VMState = runH(program, 0, input, output, 0)

  def run(state: VMState, input: List[Long], output: List[Long]): VMState = state match {
    case VMState(_, program, opInd, _, _, relOff) => runH(program, opInd, input, output, relOff)
    case _ => throw new RuntimeException
  }

  @tailrec
  private def runH(program: Map[Long, Long], opInd: Long, input: List[Long], output: List[Long], relOff: Long): VMState = {
    val (operation, p1, p2, p3) = load(program, opInd, relOff)
    operation match {
      case 1 =>
        val res = p1 + p2
        runH(program.updated(p3, res), opInd + 4, input, output, relOff)
      case 2 =>
        val res = p1 * p2
        runH(program.updated(p3, res), opInd + 4, input, output, relOff)
      case 3 =>
        if(input.isEmpty) {
          VMState(WAITING, program, opInd, input, output, relOff)
        }
        else {
          runH(program.updated(p1, input.head), opInd + 2, input.tail, output, relOff)
        }
      case 4 =>
        //println(p1)
        runH(program, opInd + 2, input, List(p1) ++ output, relOff)
      case 5 =>
        val target = if (p1 != 0) p2 else opInd + 3
        runH(program, target, input, output, relOff)
      case 6 =>
        val target = if (p1 == 0) p2 else opInd + 3
        runH(program, target, input, output, relOff)
      case 7 =>
        runH(program.updated(p3, boolToInt(p1 < p2)), opInd + 4, input, output, relOff)
      case 8 =>
        runH(program.updated(p3, boolToInt(p1 == p2)), opInd + 4, input, output, relOff)
      case 9 =>
        runH(program, opInd + 2, input, output, relOff + p1)
      case 99 =>
        VMState(TERMINATED, program, opInd, input, output, relOff)
      case _ =>
        throw new RuntimeException
    }
  }

  private def load(program: Map[Long, Long], opInd: Long, relOff: Long): (Long, Long, Long, Long) = {
    val opcode = program(opInd)
    val operation = opcode % 100
    val first = (opcode / 100) % 10
    val second = (opcode / 1000) % 10
    val third = (opcode / 10000) % 10

    val p1 = get(program, opInd, relOff, operation, first, 1)
    val p2 = get(program, opInd, relOff, operation, second, 2)
    val p3 = get(program, opInd, relOff, operation, third, 3)

    (operation, p1, p2, p3)
  }

  private def get(program: Map[Long, Long], opInd: Long, relOff: Long, operation: Long, mode: Long, pos: Long): Long = {
    if (isOutput(operation, pos)) {
      if (mode == 0)
        getI(program, opInd + pos)
      else if(mode == 2)
        getI(program, opInd + pos) + relOff
      else
        throw new RuntimeException
    }
    else {
      mode match {
        case 0 => getP(program, opInd + pos)
        case 1 => getI(program, opInd + pos)
        case 2 => getR(program, opInd + pos, relOff)
      }
    }
  }

  private def getI(program: Map[Long, Long], pos: Long): Long = program.getOrElse(pos, 0)

  private def getP(program: Map[Long, Long], pos: Long): Long = getI(program, getI(program, pos))

  private def getR(program: Map[Long, Long], pos: Long, relOff: Long): Long = getI(program, getI(program, pos) + relOff)

  private def isOutput(opcode: Long, position: Long): Boolean = {
    opcode match {
      case 1 | 2 | 7 | 8 => position == 3
      case 3 => position == 1
      case _ => false
    }
  }

  private def boolToInt(b: Boolean): Int = if (b) 1 else 0
}

sealed trait Status
case object TERMINATED extends Status
case object WAITING extends Status

case class VMState(status: Status, program: Map[Long, Long], opInd: Long, input: List[Long], output: List[Long], relOff: Long)
