import util.Util

import scala.collection.immutable.HashSet
import scala.collection.mutable

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
                 |##1#2##
                 |#######
                 |##3#4##
                 |#cB#Ab#
                 |#######""".stripMargin

  val input6 = """###############
                 |#d.ABC.#.....a#
                 |######1#2######
                 |###############
                 |######3#4######
                 |#b.....#.....c#
                 |###############""".stripMargin

  val input7 = """#############
                 |#DcBa.#.GhKl#
                 |#.###1#2#I###
                 |#e#d#####j#k#
                 |###C#3#4###J#
                 |#fEbA.#.FgHi#
                 |#############""".stripMargin

  val input8 = """#############
                 |#g#f.D#..h#l#
                 |#F###e#E###.#
                 |#dCba1#2BcIJ#
                 |#############
                 |#nK.L3#4G...#
                 |#M###N#H###.#
                 |#o#m..#i#jk.#
                 |#############""".stripMargin
  /*
  println(calc(input1))
  println(calc(input2))
  println(calc(input3))
  println(calc(input4))
   */

  //Part 1
  //println(calc(inputPart1))

  //Part 2
  println(calc2(input5))
  println(calc2(input6))
  println(calc2(input7))
  println(calc2(input8))
  //println(calc2(inputPart2))

  def calc2(input: String): Int = {
    val map = input.strip.split("\n").map(_.strip)
    val occurrences = ('a' to 'z').filter(input.contains(_))

    val chars = occurrences ++ occurrences.map(_.toUpper).filter(input.contains(_)) ++ List('1', '2', '3', '4')
    val coords = chars.map(c => {
      val index = map.mkString.indexOf(c)
      val x = index % map(0).length
      val y = index / map(0).length
      (c, x, y)
    })

    val graph = coords.map({ case (c, x, y) => (c, bfs(map, (x, y))) }).toMap
    bfs3dMultiple(graph, BFSState('1', '2', '3', '4', 0, 0), Integer.parseInt("1" * occurrences.size, 2))
  }


  case class BFSState(r1: Char, r2: Char, r3: Char, r4: Char, mask: Int, dist: Int)

  def bfs3dMultiple(graph: Map[Char, List[(Char, Int)]], start: BFSState, goal: Int): Int = {
    var visited = scala.collection.mutable.HashSet[BFSState]()
    //var toVisit = scala.collection.mutable.HashSet[BFSState](start)
    var toVisit = scala.collection.mutable.PriorityQueue[BFSState]()(Ordering.by({c => c.dist - c.mask}))
    val list = scala.collection.mutable.ArrayBuffer[BFSState]()
    toVisit.addOne(start)
    while(toVisit.nonEmpty) {
      var nextNodes = scala.collection.mutable.HashSet[BFSState]()
      for(node <- toVisit) {
        if(node.mask == goal) {
          if(list.size >= 1000) {
            return list.minBy(c => c.dist).dist
          } else {
            list.append(node)
          }
        }
        val r1 = graph(node.r1).filter(c => traversable(c._1, node.mask)).map { c =>
          BFSState(c._1, node.r2, node.r3, node.r4, node.mask, node.dist + c._2)
        }
        val r2 = graph(node.r2).filter(c => traversable(c._1, node.mask)).map { c =>
          BFSState(node.r1, c._1, node.r3, node.r4, node.mask, node.dist + c._2)
        }
        val r3 = graph(node.r3).filter(c => traversable(c._1, node.mask)).map { c =>
          BFSState(node.r1, node.r2, c._1, node.r4, node.mask, node.dist + c._2)
        }
        val r4 = graph(node.r4).filter(c => traversable(c._1, node.mask)).map { c =>
          BFSState(node.r1, node.r2, node.r3, c._1, node.mask, node.dist + c._2)
        }
        val n1 = if(node.r1.isLower) {
          BFSState(node.r1, node.r2, node.r3, node.r4, node.mask | (1 << node.r1 - 'a'), node.dist) :: r1 ++ r1.map(state => BFSState(state.r1, state.r2, state.r3, state.r4, state.mask | (1 << node.r1 - 'a'), state.dist))
        } else {
          r1
        }
        val n2 = if(node.r2.isLower) {
          BFSState(node.r1, node.r2, node.r3, node.r4, node.mask | (1 << node.r2 - 'a'), node.dist) :: r2 ++ r2.map(state => BFSState(state.r1, state.r2, state.r3, state.r4, state.mask | (1 << node.r2 - 'a'), state.dist))
        } else {
          r2
        }
        val n3 = if(node.r3.isLower) {
          BFSState(node.r1, node.r2, node.r3, node.r4, node.mask | (1 << node.r3 - 'a'), node.dist) :: r3 ++ r3.map(state => BFSState(state.r1, state.r2, state.r3, state.r4, state.mask | (1 << node.r3 - 'a'), state.dist))
        } else {
          r3
        }
        val n4 = if(node.r4.isLower) {
          BFSState(node.r1, node.r2, node.r3, node.r4, node.mask | (1 << node.r4 - 'a'), node.dist) :: r4 ++ r4.map(state => BFSState(state.r1, state.r2, state.r3, state.r4, state.mask | (1 << node.r4 - 'a'), state.dist))
        } else {
          r4
        }

        //println(node.r1 + " - " + (n1 mkString "-"))
        nextNodes.addAll(n1 ++ n2 ++ n3 ++ n4)
        visited.add(node)
      }
      toVisit = scala.collection.mutable.PriorityQueue[BFSState]()(Ordering.by({c => c.dist - c.mask}))
      toVisit.addAll(nextNodes.diff(visited).groupBy(state => (state.r1, state.r2, state.r3, state.r4, state.mask)).map(_._2.minBy(_.dist)))
    }

    0
  }

  
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
