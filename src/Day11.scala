import util.{IntcodeVMSuspendable, TERMINATED, Util, VMState, WAITING}

import scala.annotation.tailrec

object Day11 extends App {
  val software = new IntcodeVMSuspendable(Util.loadDayProgram(11))

  val initialState = software.run(List(), List())

  //Part 1
  println(robot(software, 0).keys.size)

  val picture = robot(software, 1)
  val maxX = picture.keys.map(_._1).max
  val minX = picture.keys.map(_._1).min
  val maxY = picture.keys.map(_._2).max
  val minY = picture.keys.map(_._2).min

  //Part 2
  for(y <- maxY to minY by -1) {
    for(x <- minX to maxX) {
      if(picture(x,y) == 1)
        print("#")
      else
        print(" ")
    }
    println()
  }

  def robot(software: IntcodeVMSuspendable, startingTile: Int): Map[(Int, Int), Int] = {
    val initialState = software.run(List(), List())
    robotH(software, initialState, (0, 0), 1, Map((0, 0) -> startingTile).withDefaultValue(0))
  }

  @tailrec
  def robotH(software: IntcodeVMSuspendable, state: VMState, pos: (Int, Int), heading: Int, painted: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    val camera = painted(pos)

    val newState = software.run(state, List(camera), List())

    newState.status match {
      case WAITING =>
        val color = newState.output.last
        val direction = newState.output.head

        val newHeading = turn(heading, direction.toInt)
        val newPos = move(pos, newHeading)

        robotH(software, newState, newPos, newHeading, painted.updated(pos, color.toInt))
      case TERMINATED =>
        painted
    }

  }

  //LEFT: 0, UP: 1, RIGHT: 2, DOWN 3
  def move(pos: (Int, Int), heading: Int): (Int, Int) = heading match {
    case 0 => (pos._1 - 1, pos._2)
    case 1 => (pos._1, pos._2 + 1)
    case 2 => (pos._1 + 1, pos._2)
    case 3 => (pos._1, pos._2 - 1)
  }

  def turn(heading: Int, direction: Int): Int = {
    val dir = Array(-1, 1)(direction)

    (((heading + dir) % 4 )+ 4)% 4
  }
}
