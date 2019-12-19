import util.{IntcodeVMSuspendable, Util}

import scala.annotation.tailrec

object Day19 extends App {
  val program = Util.loadDayProgram(19)
  val vm = new IntcodeVMSuspendable(program)

  val coords = (0 until 50).flatMap(x => (0 until 50).map(y => List(x.toLong, y.toLong))).toList

  //Part 1
  println(coords.flatMap(c => vm.run(c, List()).output).sum)

  //Part 2
  println(findSquare(vm))


  def findSquare(vm: IntcodeVMSuspendable): Int = {
    for(y <- 1000 until 2000) {
      val startX = LazyList.from(0).dropWhile(x => vm.run(List(x, y), List()).output.head == 0).head
      val firstX = LazyList.from(startX).dropWhile({x =>
        vm.run(List(x, y + 99), List()).output.head == 0
      }).head

      if(vm.run(List(firstX + 99, y), List()).output.head != 0) {
          return firstX*10000 + y

      }
    }
    0
  }


}
