import util.Util

import scala.annotation.tailrec

object Day22 extends App {
  val input = Util.loadDayLines(22).map(parse)

  val size = 119315717514047L
  val repetitions = 101741582076661L
  
  val (a1, b1) = combine(coefficients(10007, input, List()), 10007)

  //Part 1
  println(applyPoly(a1, b1)(2019) % 10007)

  val (a2, b2) = combine(coefficients(size, input, List()), size)
  val (aT, bT) = expBySquare(a2, b2, repetitions, size)

  val invA = aT.modInverse(size)
  val res = (((invA * 2020) % size) - (invA * bT % size)) % size

  //Part 2
  println(res)

  def expBySquare(a: BigInt, b: BigInt, n: BigInt, size: BigInt): (BigInt, BigInt) = {
    if(n == 1) {
      (a, b)
    }
    else {
      if (n % 2 == 0) {
        val (na, nb) = compose(a, b, a, b, size)
        expBySquare(na, nb, n / 2, size)
      } else {
        val (c, d) = compose(a, b, a, b, size)
        val (e, f) = expBySquare(c, d, (n - 1) / 2, size)
        compose(a, b, e, f, size)
      }
    }
  }


  def compose(a: BigInt, b: BigInt, c: BigInt, d: BigInt, size: BigInt): (BigInt, BigInt) = {
    ((a*c) % size, (a*d + b) % size)
  }

  def combine(coefficients: List[(Long, Long)], size: Long): (BigInt, BigInt) = {
    coefficients.map(c => (BigInt(c._1), BigInt(c._2))).foldLeft((BigInt(1), BigInt(0)))({case ((a, b), (c, d)) => compose(a,b,c,d,BigInt(size))})
  }

  @tailrec
  def coefficients(size: Long, instructions: List[(Int, Int)], acc: List[(Long, Long)]): List[(Long, Long)] = instructions match {
    case x::xs => x match {
      case (0, c) =>
        coefficients(size, xs, (c.toLong, 0L) :: acc)
      case (1, c) =>
        if(c >= 0) {
          coefficients(size, xs, (1L, c*(-1L)) :: acc)
        } else {
          coefficients(size, xs, (1L, c*(-1L)) :: acc)
        }
      case (2, _) =>
        coefficients(size, xs, (-1L, size -1) :: acc)
    }
    case Nil =>
      acc
  }

  def applyPoly(a: BigInt, b: BigInt)(x: BigInt): BigInt = {
    a*x + b
  }

  def parse(s: String): (Int, Int) = {
    if(s.startsWith("deal with increment")) {
      val num = s.split(" ")(3).toInt
      (0, num)
    } else if (s.startsWith("cut")) {
      val num = s.split(" ")(1).toInt
      (1, num)
    } else {
      (2, 0)
    }
  }
}
