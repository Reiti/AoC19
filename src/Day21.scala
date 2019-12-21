import util.{IntcodeVMSuspendable, Util, VMState}

object Day21 extends App {
  val program = Util.loadDayProgram(21)
  val springbot = new IntcodeVMSuspendable(program)

  val prompt = springbot.run(List(), List())

  printOutput(prompt)

  val instructionsWalk = List(
    "NOT C J",
    "AND D J",
    "NOT A T",
    "AND B T",
    "OR T J",
    "NOT B T",
    "AND C T",
    "OR T J",
    "WALK")

  val outcomeWalk = springbot.run(prompt, instructionsWalk.flatMap(stringToInput), List())

  //Part 1
  println(outcomeWalk.output.head)

  val instructionsRun = List(
    "OR E T",
    "AND I T",
    "OR H T",
    "OR H J",
    "OR I J",
    "NOT J J",
    "AND E J",
    "OR J T",
    "OR A J",
    "AND B J",
    "AND C J",
    "NOT J J",
    "AND D J",
    "AND T J",
    "RUN")

  val outcomeRun = springbot.run(prompt, instructionsRun.flatMap(stringToInput), List())

  //Part 2
  println(outcomeRun.output.head)

  def printOutput(state: VMState): Unit = {
    println(state.output.reverse.map(_.toChar).mkString)
  }

  def stringToInput(in: String): List[Long] = {
    (in+"\n").split("").map(_.head.toLong).toList
  }
}
