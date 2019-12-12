import util.Util._

import scala.annotation.tailrec

object Day12 extends App {
  val (x, y, z) = loadDay(12).split("\n").map(_.strip()).map(parse).toList.unzip3

  val (xr, yr, zr) = Iterator.iterate((x, y, z))(v => step(v._1, v._2, v._3)).drop(1000).next()

  //Part 1
  println(zip3(xr, yr, zr).map(x => totalEnergy(x._1, x._2, x._3)).sum)

  val xp = repeat(stepPlane(x), x, 0)
  val yp = repeat(stepPlane(y), y, 0)
  val zp = repeat(stepPlane(z), z, 0)

  //Part 2
  println(lcm(xp, yp, zp))


  @tailrec
  def repeat(all: List[(Int, Int)], start: List[(Int, Int)], gen: Long): Long = {
    if(all == start)
      gen + 1
    else
      repeat(stepPlane(all), start, gen+1)
  }

  def parse(s: String) = {
    val split = s.split(", ")
    val pos = split.map(_.split("=")(1).stripSuffix(">").toInt)

    ((pos(0), 0), (pos(1), 0), (pos(2), 0))
  }

  def step(x: List[(Int, Int)], y: List[(Int, Int)], z: List[(Int, Int)]) = {
    (stepPlane(x), stepPlane(y), stepPlane(z))
  }

  def potentialEnergy(x: (Int, Int), y: (Int, Int), z: (Int, Int)): Int = {
    Math.abs(x._1) + Math.abs(y._1) + Math.abs(z._1)
  }

  def kineticEnergy(x: (Int, Int), y: (Int, Int), z: (Int, Int)): Int = {
    Math.abs(x._2) + Math.abs(y._2) + Math.abs(z._2)
  }

  def totalEnergy(x: (Int, Int), y: (Int, Int), z: (Int, Int)): Int = potentialEnergy(x,y,z) * kineticEnergy(x,y,z)

  def stepPlane(all: List[(Int, Int)]): List[(Int, Int)] = {
    all map { m =>
      val x = all.map(n => if(n._1 == m._1) 0 else if(n._1 > m._1) 1 else -1).sum
      (m._1 + m._2 + x, m._2 +x)
    }
  }
}
