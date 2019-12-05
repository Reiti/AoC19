import util.Util

object Day3 {
  def main(args: Array[String]): Unit = {
    val wires = Util.loadDay(3).strip().split("\n").map(_.strip().split(",").toList)

    val w1 = points(wires(0), 0, 0, 0).tail
    val w2 = points(wires(1), 0, 0, 0).tail

    val intersections = w1.map(x => (x._1, x._2)).toSet.intersect(w2.map(x => (x._1, x._2)).toSet)

    //Part 1
    println(intersections.map({case (x, y) => Math.abs(x) + Math.abs(y)}).min)

    val intersectionsWithSteps = intersections.map({case (x, y) =>
      val one = w1.find(p => p._1 == x && p._2 == y )
      val two = w2.find(p => p._1 == x && p._2 == y)
      (x, y, one.get._3 + two.get._3)
    })

    //Part 2
    println(intersectionsWithSteps.map(_._3).min)
  }

  def parseDirection(dir: String): (Char, Int) = {
    (dir.charAt(0), dir.substring(1).toInt)
  }

  def points(wire: List[String], x: Int, y: Int, steps: Int): List[(Int, Int, Int)] = wire match {
    case head::xs =>
      val (xOff, yOff) = parseDirection(head) match {
        case ('R', dist) => (dist, 0)
        case ('L', dist) => (-dist, 0)
        case ('U', dist) => (0, dist)
        case ('D', dist) => (0, -dist)
        case _ => (0, 0)
      }

      val distance = parseDirection(head)._2
      val stepX = if(xOff.sign == 0){1} else xOff.sign
      val stepY = if(yOff.sign == 0){1} else yOff.sign
      val newPoints = for{
        xc:Int <- x to (x + xOff) by stepX
        yc:Int <- y to (y + yOff) by stepY
      } yield(xc, yc)

      val withDist = newPoints.zipWithIndex.map({case ((x, y), i) => (x, y, steps + i)})
      withDist.toList ++ points(xs, x + xOff, y + yOff, steps + distance)
    case Nil => List()
  }


}
