import util.Util

import scala.annotation.tailrec

object Day6 {
  def main(args: Array[String]): Unit = {
    val inputs = Util.loadDayLines(6).map(x => (x.split("[)]")(0), x.split("[)]")(1)))
    val parents = inputs.map(x => (x._2, x._1)).toMap
    val objects = inputs.map(_._2).distinct

    //Part 1
    println(objects.map(x => orbits(parents, x).size).sum)

    val we = orbits(parents, "YOU")
    val santa  = orbits(parents, "SAN")

    //Part 2
    println(we.size + santa.size - (2 * we.toSet.intersect(santa.toSet).size))
  }

  def orbits(parents: Map[String, String], obj: String): List[String] = {
    @tailrec
    def orbitsH(parents: Map[String, String], obj: String, acc: List[String]): List[String] = parents.get(obj) match {
      case Some(x) => orbitsH(parents, x, List(x) ++ acc)
      case None => acc
    }
    orbitsH(parents, obj, List())
  }
}
