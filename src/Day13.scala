import util.{IntcodeVMSuspendable, TERMINATED, Util, VMState}

import scala.annotation.tailrec

object Day13 extends App {
  val program  = Util.loadDayProgram(13)
  val vm = new IntcodeVMSuspendable(program)

  //Part 1
  println(vm.run(List(), List()).output.grouped(3).count(_.head == 2))

  val vmFree = new IntcodeVMSuspendable(program.updated(0, 2))

  //Part 2
  println(play(vmFree))

  def play(vm: IntcodeVMSuspendable): Long = {
    val newState = vm.run(List(), List())
    playH(vm, newState, 0, 0)
  }

  @tailrec
  def playH(vm: IntcodeVMSuspendable, state: VMState, pxOld: Int, scoreOld: Int): Int = {
    val out = state.output.grouped(3).toList
    if(state.status == TERMINATED) {
      out.find(_ (2) == -1) match {
        case Some(v) => v.head.toInt
        case None => scoreOld
      }
    }
    else {
      val bx = out.find(x => x(2) != -1 && x.head == 4).get(2)
      val px = out.find(x => x(2) != -1 && x.head == 3) match {
        case Some(v) => v(2)
        case None => pxOld
      }
      val score = out.find(x => x(2) == -1) match {
        case Some(v) => v.head
        case None => scoreOld
      }

      val direction = Math.signum(bx - px).toInt
      playH(vm, vm.run(state, List(direction), List()), px.toInt, score.toInt)
    }
  }
}
