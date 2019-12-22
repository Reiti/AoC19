import util.Util

import scala.annotation.tailrec

object Day22 extends App {
  val input = Util.loadDayLines(22).map(parse)

  val size = 119315717514047L
  val repetitions = 101741582076661L

  //Part 1
  println(calcPos(10007, input, 2019))

  val (c, d) = combine(coefficients(size, input, List()), size)

  val (a, b) = expBySquare(c, d, repetitions, size)

  val invA = a.modInverse(size)
  val res = (((invA * 2020) % size) - (invA * b % size)) % size

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

  @tailrec
  def calcPos(size: Long, instructions: List[(Int, Int)], pos: Long): Long = instructions match {
    case x::xs => x match {
      case (0, c) =>
        calcPos(size, xs, (pos * c) % size)
      case (1, c) =>
        if(c >= 0) {
          if(c > pos) {
            calcPos(size, xs, (size - c) + pos)
          } else {
            calcPos(size, xs, pos - c)
          }
        } else {
          if((size + c) > pos) {
            calcPos(size, xs, pos - c)
          } else {
            calcPos(size, xs, pos - (size + c))
          }
        }
      case (2, _) =>
        calcPos(size, xs, size - pos - 1)
    }
    case Nil =>
      pos
  }

  @tailrec
  def shuffle(stack: List[Int], instructions: List[(Int, Int)]): List[Int] = instructions match {
    case x::xs =>
      x match {
        case (0, c) =>
          shuffle(dealNew(stack, c), xs)
        case (1, c) =>
          if(c >= 0) {
            val p1 = stack.slice(0, c)
            val p2 = stack.slice(c, stack.size)
            shuffle(p2 ++ p1, xs)
          }  else {
            val p1 = stack.slice(0, stack.size + c)
            val p2 = stack.slice(stack.size + c, stack.size)
            shuffle(p2 ++ p1, xs)
          }
        case (2, _) =>
          shuffle(stack.reverse, xs)
      }
    case Nil =>
      stack
  }

  def dealNew(stack: List[Int], inc: Int): List[Int] = {
    val size = stack.size
    @tailrec
    def dealNewH(stack: List[Int], inc: Int, acc: List[Int], pos: Int): List[Int] = stack match {
      case x::xs => dealNewH(xs, inc, acc.updated(pos, x), (pos + inc) % size)
      case Nil => acc
    }
    dealNewH(stack, inc, List.fill(size){0}, 0)
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
