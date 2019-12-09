import util.{IntcodeVM, Pipe, Util}

object Day9 extends App{
  val program = Util.loadDayProgram(9)
  val vm = new IntcodeVM(program)

  val in1 = new Pipe
  val out1 = new Pipe

  in1.write(1)

  //Part 1
  println(vm.run(in1, out1).readLast())

  val in2 = new Pipe
  val out2 = new Pipe

  in2.write(2)

  //Part 2
  println(vm.run(in2, out2).readLast())
}
