import util.{IntcodeVMSuspendable, Util, VMState}

import scala.annotation.tailrec

object Day25 extends App {
  val program = Util.loadDayProgram(25)
  val vm = new IntcodeVMSuspendable(program)
  val initialState = vm.run(List(), List())


  //Solved by playing the game :)
  runGame(vm, initialState)


  @tailrec
  def runGame(vm: IntcodeVMSuspendable, state: VMState): Unit = {
    printOutput(state)
    val input = scala.io.StdIn.readLine()
    val newS = vm.run(state, input.split("").map(_(0).toLong).appended('\n'.toLong).toList, List())
    runGame(vm, newS)
  }

  def printOutput(state: VMState): Unit = {
    println(state.output.reverse.map(_.toChar).mkString)
  }
}
