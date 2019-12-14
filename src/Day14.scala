import sun.util.locale.provider.AvailableLanguageTags
import util.Util

import scala.annotation.tailrec

case class Component(name: String, quantity: Long)
case class Reaction(component: Component, parts: List[Component])

object Day14 extends App   {
  val reactions = Util.loadDay(14).split("\n").map(parse).toList

  val input1 = """10 ORE => 10 A
                 |1 ORE => 1 B
                 |7 A, 1 B => 1 C
                 |7 A, 1 C => 1 D
                 |7 A, 1 D => 1 E
                 |7 A, 1 E => 1 FUEL""".stripMargin.split("\n").map(parse).toList
  val input2 = """9 ORE => 2 A
                 |8 ORE => 3 B
                 |7 ORE => 5 C
                 |3 A, 4 B => 1 AB
                 |5 B, 7 C => 1 BC
                 |4 C, 1 A => 1 CA
                 |2 AB, 3 BC, 4 CA => 1 FUEL""".stripMargin.split("\n").map(parse).toList
  val input3 = """157 ORE => 5 NZVS
                 |165 ORE => 6 DCFZ
                 |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
                 |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
                 |179 ORE => 7 PSHF
                 |177 ORE => 5 HKGWZ
                 |7 DCFZ, 7 PSHF => 2 XJWVT
                 |165 ORE => 2 GPVTF
                 |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT""".stripMargin.split("\n").map(parse).toList
  val input4 = """2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
                 |17 NVRVD, 3 JNWZP => 8 VPVL
                 |53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
                 |22 VJHF, 37 MNCFX => 5 FWMGM
                 |139 ORE => 4 NVRVD
                 |144 ORE => 7 JNWZP
                 |5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
                 |5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
                 |145 ORE => 6 MNCFX
                 |1 NVRVD => 8 CXFTF
                 |1 VJHF, 6 MNCFX => 4 RFSQX
                 |176 ORE => 6 VJHF""".stripMargin.split("\n").map(parse).toList

  val input5 ="""171 ORE => 8 CNZTR
                      |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
                      |114 ORE => 4 BHXH
                      |14 VRPVC => 6 BMBT
                      |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
                      |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
                      |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
                      |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
                      |5 BMBT => 4 WPTQ
                      |189 ORE => 9 KTJDG
                      |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
                      |12 VRPVC, 27 CNZTR => 2 XDBXC
                      |15 KTJDG, 12 BHXH => 5 XCVML
                      |3 BHXH, 2 VRPVC => 7 MZWV
                      |121 ORE => 7 VRPVC
                      |7 XCVML => 6 RJRHP
                      |5 BHXH, 4 VRPVC => 5 LTCX""".stripMargin.strip.split("\n").map(parse).toList




    println(produce(input1, Map("FUEL" -> 1L)))
    println(produce(input2, Map("FUEL" -> 1L)))
    println(produce(input3, Map("FUEL" -> 1L)))
    println(produce(input4, Map("FUEL" -> 1L)))
    println(produce(input5, Map("FUEL" -> 1L)))


  @tailrec
  def produce(reactions: List[Reaction], available: Map[String, Long]): Long = {
    if(available.size == 1 && available.contains("ORE")) {
      available("ORE")
    } else {
      val stepped = step(reactions, available)

      val mat = stepped.keys.toList.filter(_ != "ORE").head

      val  rec = reactions.find(r => r.component.name == mat).get

      val updatedStepped = update(rec.parts, stepped.removed(mat), 1)
      produce(reactions, updatedStepped)
    }
  }
  def depth(reactions: List[Reaction], curr: Reaction, toFind: String): Int = {
    if(curr.component.name == toFind) {
      0
    }
    else {
      val found = reactions.filter(r => curr.parts.exists(c => r.component.name == c.name))
      if(found.isEmpty)
        0
      else
        1 + found.map(r => depth(reactions, r, toFind)).reduce(Math.max)
    }
  }

  @tailrec
  def step(reactions: List[Reaction], available: Map[String, Long]): Map[String, Long] = {
    val next = available.find(x => producible(reactions, x))
    next match {
      case Some(v) =>
        val n = v._1
        val q = v._2
        val r = reactions.find(r => r.component.name == n).get
        val total = q / r.component.quantity
        val remainder = q % r.component.quantity

        val newAvailable = if(remainder == 0) {
          available.removed(n)
        } else {
          available.updated(n, remainder)
        }

        val nextAvailable = update(r.parts, newAvailable, total)
        step(reactions, nextAvailable)
      case None =>
        available
    }
  }

  @tailrec
  def update(parts: List[Component], available: Map[String, Long], quant: Long): Map[String, Long] = parts match {
    case x :: xs =>
      val q = available.getOrElse(x.name, 0L)
      update(xs, available.updated(x.name, q + (x.quantity * quant)), quant)
    case Nil =>
    available
  }
  def producible(reactions: List[Reaction], curr: (String, Long)): Boolean = {
    if(curr._1 == "ORE")
      false
    else {
      val q = reactions.find(x => x.component.name == curr._1).get.component.quantity
      (q <= curr._2)
    }
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

