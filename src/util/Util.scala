package util

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Util {
  def loadDay(day: Int): String = {
    Using(Source.fromResource(s"$day.txt")) {
      source => source.mkString.strip()
    }.get
  }

  def loadDayLines(day: Int): List[String] = loadDay(day).split("\n").toList

  def loadDayInts(day: Int): List[Int] = loadDayLines(day).map(_.toInt)

  def loadDayProgram(day: Int): Map[Long, Long] =
    Util.loadDay(day)
    .split(",")
    .map { x => x.toLong }
    .zipWithIndex
    .map { x => (x._2.toLong, x._1) }.toMap

  @tailrec
  def gcd(a: Long, b: Long): Long = {
    if(b == 0)
      a
    else
      gcd(b, a%b)
  }

  def lcm(a: Long, b: Long): Long = a * b / gcd(a, b)

  def lcm(nums: Long*): Long = nums.reduce(lcm)

  def zip3[A](a: List[A], b: List[A], c: List[A]): List[(A, A, A)] = {
    (a zip b zip c) map {x => x match {
      case ((a, b), c) => (a, b, c)
    }}
  }
}

