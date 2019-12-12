import util.Util._

import scala.annotation.tailrec

object Day12 extends App {
  val (x, y, z) = loadDay(12).split("\n").map(_.strip()).map(parse).toList.unzip3

  //Part 1
  println(zip3(Iterator.iterate((x, y, z))(step).drop(1000).next()).map(totalEnergy).sum)

  //Part 2
  println(lcm(repeat(x), repeat(y), repeat(z)))

  def repeat(start: List[(Int, Int)]): Long = repeatH(start, start, 0)

  @tailrec
  def repeatH(all: List[(Int, Int)], start: List[(Int, Int)], gen: Long): Long = {
    val next = stepPlane(all)
    if(next == start)
      gen + 1
    else
      repeatH(next, start, gen+1)
  }

  def parse(s: String) = {
    val split = s.split(", ")
    val pos = split.map(_.split("=")(1).stripSuffix(">").toInt)

    ((pos(0), 0), (pos(1), 0), (pos(2), 0))
  }

  def step(t:(List[(Int, Int)], List[(Int, Int)], List[(Int, Int)])) = {
    (stepPlane(t._1), stepPlane(t._2), stepPlane(t._3))
  }

  def potentialEnergy(x: (Int, Int), y: (Int, Int), z: (Int, Int)): Int = {
    Math.abs(x._1) + Math.abs(y._1) + Math.abs(z._1)
  }

  def kineticEnergy(x: (Int, Int), y: (Int, Int), z: (Int, Int)): Int = {
    Math.abs(x._2) + Math.abs(y._2) + Math.abs(z._2)
  }

  def totalEnergy(t: ((Int, Int), (Int, Int), (Int, Int))): Int = potentialEnergy(t._1,t._2,t._3) * kineticEnergy(t._1,t._2,t._3)

  def stepPlane(all: List[(Int, Int)]): List[(Int, Int)] = {
    all map { m =>
      val x = all.map(n => if(n._1 == m._1) 0 else if(n._1 > m._1) 1 else -1).sum
      (m._1 + m._2 + x, m._2 +x)
    }
  }
}
