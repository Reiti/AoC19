import util.Util

import scala.collection.immutable.HashSet

object Day18 extends App {

  val inputPart1 = Util.loadDay(18)
  val inputPart2 = Util.loadFilename("18_2.txt")

  val input1 = """########################
                |#f.D.E.e.C.b.A.@.a.B.c.#
                |######################.#
                |#d.....................#
                |########################""".stripMargin



  val input2 = """########################
                |#...............b.C.D.f#
                |#.######################
                |#.....@.a.B.c.d.A.e.F.g#
                |########################""".stripMargin

  val input3 = """#################
                |#i.G..c...e..H.p#
                |########.########
                |#j.A..b...f..D.o#
                |########@########
                |#k.E..a...g..B.n#
                |########.########
                |#l.F..d...h..C.m#
                |#################""".stripMargin




  val input4 = """########################
                |#@..............ac.GI.b#
                |###d#e#f################
                |###A#B#C################
                |###g#h#i################
                |########################""".stripMargin

  val input5 = """#######
                 |#a.#Cd#
                 |##@#@##
                 |#######
                 |##@#@##
                 |#cB#Ab#
                 |#######""".stripMargin

  /*
  println(calc(input1))
  println(calc(input2))
  println(calc(input3))
  println(calc(input4))
   */

  //Part 1
  println(calc(inputPart1))

  //Part 2


  def calc(input: String): Int = {
    val map = input.strip.split("\n").map(_.strip)

    val occurrences = ('a' to 'z').filter(input.contains(_))

    val chars = occurrences ++ occurrences.map(_.toUpper).filter(input.contains(_)) ++ List('@')
    val coords = chars.map(c => {
      val index = map.mkString.indexOf(c)
      val x = index % map(0).length
      val y = index / map(0).length
      (c, x, y)
    })
    val graph = coords.map({ case (c, x, y) => (c, bfs(map, (x, y))) }).toMap

    bfs3d(graph, '@', Integer.parseInt("1" * occurrences.size, 2))
  }

  def bfs3d(graph: Map[Char, List[(Char, Int)]], start: Char, goal: Int): Int = {
    val visited = scala.collection.mutable.HashSet[(Char, Int)]()
    var toVisit = scala.collection.mutable.HashSet[(Char, Int, Int)]((start, 0, 0))
    var min = Integer.MAX_VALUE
    while(toVisit.nonEmpty) {
      val newNodes = scala.collection.mutable.HashSet[(Char, Int, Int)]()
      for(node <- toVisit) {
        if(node._3 == goal) {
          if(node._2 < min) {
            min = node._2
          }
        }
        val neighbors = graph(node._1).filter(c => !visited.contains((c._1, node._3)) && traversable(c._1, node._3)).map(c => (c._1, c._2 + node._2, node._3))
        val consider = if(node._1 != '@' && node._1.isLower) {
          (node._1, node._2, node._3 | (1 << node._1.toChar - 'a')) :: neighbors
        } else {
          neighbors
        }

        val valid = consider.filter(c => !visited.contains((c._1, c._3)))
        newNodes.addAll(valid)
        visited.add((node._1, node._3))
      }

      toVisit = newNodes
    }
    min
  }

  def traversable(c: Char, mask: Int): Boolean = {
    if(c.isLower || c == '@')
      true
    else {
      val bit = c.toLower.toInt - 'a'.toInt
      (mask & (1 << bit)) != 0
    }
  }

  def bfs(map: Array[String], start: (Int, Int)): List[(Char, Int)] = {
    val visited = scala.collection.mutable.HashSet[(Int, Int)]()
    var toVisit = scala.collection.mutable.HashSet[(Int, Int)](start)
    var depth = 0
    val list = scala.collection.mutable.ArrayBuffer[(Char, Int)]()

    while(toVisit.nonEmpty) {
      val newNodes = scala.collection.mutable.HashSet[(Int, Int)]()
      for(node <- toVisit) {
        val valid = neighborsPlane(node).filter({case (x, y) => map(y)(x) != '#' && !visited.contains((x, y))})
        newNodes.addAll(valid)

        for(pos <- valid) {
          val c = map(pos._2)(pos._1)
          if(c != '.') {
            list.append((c, depth + 1))
            if(c.isUpper) {
              newNodes.remove((pos._1, pos._2))
            }
          }
        }
        visited.add(node)
      }
      toVisit = newNodes
      depth = depth + 1
    }
    list.toList
  }

  def neighborsPlane(pos: (Int, Int)) = {
    val u = (pos._1, pos._2 - 1)
    val d = (pos._1, pos._2 + 1)
    val l = (pos._1 - 1, pos._2)
    val r = (pos._1 + 1, pos._2)
    List(u, d, l, r)
  }

}
