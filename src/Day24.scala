import util.Util

import scala.annotation.tailrec

object Day24 extends App {
  val input = Util.loadDayLines(24).toArray

  //Part 1
  println(findRepetition(input, Set(), 0))

  val recursiveGrid = (-100 to 100).map(level => (level, Array.fill(5)("."*5))).toMap.updated(0, input)

  val evolvedOnce = evolveStructure(recursiveGrid)

  val twoHundredth = Iterator.iterate(recursiveGrid)(evolveStructure).drop(200).next()

  //Part 2
  println(countBugs(twoHundredth))

  def countBugs(map: Map[Int, Array[String]]): Int = {
    map.map(l => l._2.mkString.count(_ == '#')).sum
  }

  def evolveStructure(map: Map[Int, Array[String]]): Map[Int, Array[String]] = {
    map.map(l => (l._1, evolveLayer(map, l._1)))
  }

  def evolveLayer(map: Map[Int, Array[String]], layer: Int): Array[String] = {
    val res = map(layer).zipWithIndex map { case (line, y) =>
      line.zipWithIndex map { case (char, x) =>
        if (x == 2 && y == 2) {
          '.'
        }
        else {
          if (char == '#') {
            if (countRecursiveNeighbors(map, layer, x, y) == 1) {
              '#'
            } else {
              '.'
            }
          } else {
            if (countRecursiveNeighbors(map, layer, x, y) == 1 || countRecursiveNeighbors(map, layer, x, y) == 2) {
              '#'
            }
            else {
              '.'
            }
          }
        }
      }
    }
    res.map(_.mkString)
  }

  def countRecursiveNeighbors(map: Map[Int, Array[String]], level: Int, x: Int, y: Int): Int = {
    val curr = map(level).map(_.lift).lift
    val u = count(curr(y-1) flatMap {_(x)} getOrElse '.')
    val d = count(curr(y+1) flatMap {_(x)} getOrElse '.')
    val r = count(curr(y) flatMap {_(x+1)} getOrElse '.')
    val l = count(curr(y) flatMap {_(x-1)} getOrElse '.')

    if(x == 0 && y == 4 && level > -100) {
      val outer = count(map(level-1)(2)(1)) + count(map(level-1)(3)(2))
      outer + u + r
    } else if(x == 0 && y == 0 && level > -100) {
      val outer = count(map(level-1)(2)(1)) + count(map(level-1)(1)(2))
      outer + r + d
    } else if(x == 4 && y == 0 && level > -100) {
      val outer = count(map(level-1)(1)(2)) + count(map(level-1)(2)(3))
      outer + l + d
    } else if(x == 4 && y == 4 && level > -100) {
      val outer = count(map(level-1)(2)(3)) + count(map(level-1)(3)(2))
      outer + l + u
    } else if(x == 0 && level > -100) {
      val outer = count(map(level-1)(2)(1))
      outer + u + d + r
    } else if(x == 4 && level > -100) {
      val outer = count(map(level-1)(2)(3))
      outer + u + d + l
    } else if(y == 0 && level > -100) {
      val outer = count(map(level-1)(1)(2))
      outer + l + r + d
    } else if(y == 4 && level > -100) {
      val outer = count(map(level-1)(3)(2))
      outer + l + r + u
    } else if(x == 2 && y == 1 && level < 100) {
      val inner = map(level+1)(0).map(count).sum
      inner + u + l + r
    } else if(x == 1 && y == 2 && level < 100) {
      val inner = map(level+1).map(_.head).map(count).sum
      inner + u + l + d
    } else if(x == 3 && y == 2 && level < 100) {
      val inner = map(level+1).map(_(4)).map(count).sum
      inner + u + r + d
    } else if(x == 2 && y == 3 && level < 100) {
      val inner = map(level+1)(4).map(count).sum
      inner + r + d + l
    } else {
      u + r + d + l
    }
  }

  @tailrec
  def findRepetition(input: Array[String], occ: Set[Int], acc: Int): Int = {
    if (occ.contains(hash(input))) {
      hash(input)
    } else {
      val newM = input.zipWithIndex map { case (line, y) =>
        line.zipWithIndex map { case (char, x) =>
          if (char == '#') {
            if (countNeighbors(input, x, y) == 1) {
              '#'
            } else {
              '.'
            }
          } else {
            if (countNeighbors(input, x, y) == 1 || countNeighbors(input, x, y) == 2) {
              '#'
            }
            else {
              '.'
            }
          }
        }
      }
      findRepetition(newM.map(_.mkString), occ union Set(hash(input)), acc + 1)
    }
  }

  def hash(map: Array[String]): Int = {
    Integer.parseInt(map.mkString("").reverse.map(x => if(x == '#' ) '1' else '0'), 2)
  }

  def countNeighbors(map: Array[String], x: Int, y: Int): Int = {
    val curr = map.map(_.lift).lift
    val u = count(curr(y-1) flatMap {_(x)} getOrElse '.')
    val d = count(curr(y+1) flatMap {_(x)} getOrElse '.')
    val r = count(curr(y) flatMap {_(x+1)} getOrElse '.')
    val l = count(curr(y) flatMap {_(x-1)} getOrElse '.')
    u + d + r + l
  }

  def count(char: Char): Int = {
    if (char == '#')
      1
    else
      0
  }
}
