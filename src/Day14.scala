import util.Util

import util.Util.{Graph, Node}

import scala.annotation.tailrec

case class Component(name: String, quantity: Long)
case class Reaction(component: Component, parts: List[Component])

object Day14 extends App   {
  val reactions = Util.loadDay(14).split("\n").map(parse).toList

  //Part 1
  println(calculateOreCost(reactions, 1))

  //Part 2
  println(binarySearch(reactions, 0L, 1000000000L))

  @tailrec
  def binarySearch(reactions: List[Reaction], low: Long, high: Long): Long = {
    val check = low + (high - low) / 2L
    val needed = calculateOreCost(reactions, check)
    val neededPlusOne = calculateOreCost(reactions, check + 1)
    if(needed <= 1000000000000L && neededPlusOne > 1000000000000L) {
      check
    } else if (needed <=  1000000000000L) {
      binarySearch(reactions, check, high)
    } else {
      binarySearch(reactions, low, check)
    }
  }

  def calculateOreCost(reactions: List[Reaction], amount: Long): Long = {
    val ore = List((Node("ORE"), Set[Node]()))
    val parsed = (ore ++ reactions.map(r => (Node(r.component.name), r.parts.map(x => Node(x.name)).toSet))).toMap
    val graph = Graph(parsed)
    //Double reversal with distinct only leaves the last occurence of a node in the list
    work(reactions, graph.topologicalSort.get.reverse.distinct.reverse.map(_.label).filter(_ != "ORE"), Map("FUEL" -> amount))
  }

  @tailrec
  def work(reactions: List[Reaction], sorted: List[String], cost: Map[String, Long]): Long = sorted match {
    case x::xs =>
      val r = reactions.find(r => r.component.name == x).get
      val rq = r.component.quantity
      val cq = cost(x)
      val total =  Math.ceil(cq.toDouble / rq.toDouble).toLong
      work(reactions, xs, addCost(r.parts, cost, total).removed(x))
    case Nil =>
      cost("ORE")
  }

  @tailrec
  def addCost(parts: List[Component], cost: Map[String, Long], quant: Long): Map[String, Long] = parts match {
    case x::xs =>
      val curr = cost.getOrElse(x.name, 0L)
      addCost(xs, cost.updated(x.name, curr + x.quantity*quant), quant)
    case Nil =>
      cost
  }

  def parse(in: String): Reaction = {
    val split = in.strip.split(" => ")

    val end = parseComponent(split(1))
    val req = split(0).split(", ").map(parseComponent).toList

    Reaction(end, req)
  }

  def parseComponent(in: String): Component = {
    val split = in.split(" ")
    Component(split(1), split(0).toInt)
  }
}

