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
      if(picture.getOrElse((x, y), 0) == 1)
        print("#")
      else
        print(" ")
    }
    println()
  }

  def robot(software: IntcodeVMSuspendable, startingTile: Int): Map[(Int, Int), Int] = {
    val initialState = software.run(List(), List())
    robotH(software, initialState, (0, 0), 2, Map((0, 0) -> startingTile))
  }

  @tailrec
  def robotH(software: IntcodeVMSuspendable, state: VMState, pos: (Int, Int), heading: Int, painted: Map[(Int, Int), Int]): Map[(Int, Int), Int] = {
    val camera = painted.get(pos) match {
      case Some(x) => x
      case None => 0
    }

    val newState = software.run(state, List(camera), List())

    newState.status match {
      case WAITING =>
        val color = newState.output.last
        val direction = newState.output.head
        val (newPos, newHeading) = turn(pos, heading, direction.toInt)

        robotH(software, newState, newPos, newHeading, painted.updated(pos, color.toInt))
      case TERMINATED =>
        painted
    }

  }

  //LEFT: 0, RIGHT: 1, UP: 2, DOWN: 3
  def turn(pos: (Int, Int), heading: Int, direction: Int): ((Int, Int), Int) = {
    heading match {
      case 0 =>
        if(direction == 0) {
          ((pos._1, pos._2 - 1), 3)
        }
        else {
          ((pos._1, pos._2 + 1), 2)
        }
      case 1 =>
        if(direction == 0) {
          ((pos._1, pos._2 + 1), 2)
        }
        else {
          ((pos._1, pos._2 - 1), 3)
        }
      case 2 =>
        if(direction == 0) {
          ((pos._1 - 1, pos._2), 0)
        }
        else {
          ((pos._1 + 1, pos._2), 1)
        }
      case 3 =>
        if(direction == 0) {
          ((pos._1 + 1, pos._2), 1)
        }
        else {
          ((pos._1 - 1, pos._2), 0)
        }
    }
  }
}
