import util.Util

import scala.annotation.tailrec

object Day16 extends App{
  val input = Util.loadDay(16).strip.split("").map(_.toInt).toList
  //val input = """03036732577212944063491565474664""".strip.split("").map(_.toInt).toList

  val size = input.size
  val repeated = (1 to size).map{times => Vector(0, 1, 0, -1).flatMap(value => Vector.fill(times) {
    value
  })}.toVector

  val pattern = repeated.map(p => LazyList.continually(p).flatten.take(size + 1).toList.tail.toVector).toVector

  //Part 1
  println(Iterator.iterate(input.toVector)(prev => mul(prev, pattern)).drop(100).next.take(8).mkString)

  val offset = input.take(7).mkString.toInt

  val realInput = Array.fill(10000)(input).flatten.drop(offset)

  //Part 2
  println(Iterator.iterate(realInput)(efficient).drop(100).next().take(8).mkString)

  def mul(vec: Vector[Int], mat: Vector[Vector[Int]]): Vector[Int] = {
    val res = for(row <- vec.indices) yield {
      val curr = for(col <- vec.indices) yield {
        val value = mat(row)(col)
        vec(col) * value
      }

      Math.abs(curr.sum) % 10
    }

    res.toVector
  }

  def efficient(in: Array[Int]): Array[Int] = {
    val res = in.reverse.foldLeft((List[Int](), 0))({ case (acc, curr) =>
      val elem = (curr + acc._2) % 10
      (List(elem) ++ acc._1, elem)
    })
    res._1.toArray
  }
}
