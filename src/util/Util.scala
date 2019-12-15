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

  def zip3[A](t: (List[A], List[A], List[A])): List[(A, A, A)] = zip3(t._1, t._2, t._3)

  def time[A](block: => A): A = {
    val t0  = System.currentTimeMillis()
    val result = block
    val t1 = System.currentTimeMillis()
    println("Elapsed time: " + (t1 - t0) + "ms")
    result
  }

  case class Node(label: String)
  case class Graph(adj: Map[Node, Set[Node]]) {
    case class DfsState(discovered: Set[Node] = Set(), activeNodes: Set[Node] = Set(), tsOrder: List[Node] = List(),
                        isCylic: Boolean = false)

    def dfs: (List[Node], Boolean) = {
      def dfsVisit(currState: DfsState, src: Node): DfsState = {
        val newState = currState.copy(discovered = currState.discovered + src, activeNodes = currState.activeNodes + src,
          isCylic = currState.isCylic || adj(src).exists(currState.activeNodes))

        val finalState = adj(src).filterNot(newState.discovered).foldLeft(newState)(dfsVisit)
        val ord = if(finalState.tsOrder.contains(src)) finalState.tsOrder else src :: finalState.tsOrder
        finalState.copy(tsOrder = ord, activeNodes = finalState.activeNodes - src)
      }

      val stateAfterSearch = adj.keys.foldLeft(DfsState()) {(state, n) => if (state.discovered(n)) state else dfsVisit(state, n)}
      (stateAfterSearch.tsOrder, stateAfterSearch.isCylic)
    }

    def topologicalSort: Option[List[Node]] = dfs match {
      case (topologicalOrder, false) => Some(topologicalOrder)
      case _ => None
    }
  }
}

