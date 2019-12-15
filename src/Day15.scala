import util.{IntcodeVMSuspendable, Util, VMState}

import scala.annotation.tailrec

object Day15 extends App{
  val program = Util.loadDayProgram(15)

  val vm = new IntcodeVMSuspendable(program)
  val state = vm.run(List(), List())
  var visited: Map[(Int, Int), Int] = Map()

  val res = (1 to 4).map(dir => search(state, vm, (0, 0), dir, 0)).reduce(Math.min)

  //Part 1
  println(res)

  //Part 2
  println(expand(0))

  @tailrec
  def expand(time: Int): Int = {
    val locs = visited.filter(loc => visited.getOrElse(loc._1, 0) == 1 && ({
      val up = move(loc._1, 1)
      val down = move(loc._1, 2)
      val left = move(loc._1, 3)
      val right = move(loc._1, 4)

      val u = visited.getOrElse(up, 0)
      val d = visited.getOrElse(down, 0)
      val l = visited.getOrElse(left, 0)
      val r = visited.getOrElse(right, 0)

      (u == 2 || d == 2 || l == 2 || r == 2)
    }
    ))

    if(locs.isEmpty) {
      time
    }
    else {
      for (loc <- locs) {
        visited = visited.updated(loc._1, 2)
      }
      expand(time + 1)
    }
  }

  def search(state: VMState, vm: IntcodeVMSuspendable, location: (Int, Int), direction: Int, length: Int): Int = {
    val newState = vm.run(state, List(direction), List())

    val output = newState.output.head
    val newLoc = move(location, direction)
    output match {
      case 0 =>
        visited = visited.updated(newLoc, output.toInt)
        Int.MaxValue
      case 1 =>
        if(visited.contains(newLoc)) {
          Int.MaxValue
        }
        else {
          visited = visited.updated(newLoc, output.toInt)
          (1 to 4).map(dir => search(newState, vm, newLoc, dir, length + 1)).reduce(Math.min)
        }
      case 2 =>
        visited = visited.updated(newLoc, output.toInt)
        length + 1
    }
  }

  def printMap(): Unit = {
    visited foreach println
  }
  def move(location: (Int, Int), direction: Int): (Int, Int) = {
    direction match {
      case 1 => (location._1, location._2 + 1)
      case 2 => (location._1, location._2 - 1)
      case 3 => (location._1 - 1, location._2)
      case 4 => (location._1 + 1, location._2)
    }
  }

}
