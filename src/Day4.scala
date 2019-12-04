object Day4 {
  def main(args: Array[String]): Unit = {
    val input = (254032 to 789860).map(_.toString)
    val increasing = input.filter(checkIncreasing)

    //Day 1
    println(increasing.count(checkDouble))

    //Day 2
    println(increasing.count(checkOnlyDouble))
  }

  def checkDouble(p: String): Boolean = {
    p.toSet.size != p.length
  }

  def checkIncreasing(p: String): Boolean = {
    val l = p.toList.map(_.toInt)
    l zip l.tail forall {x => x._1 <= x._2}
  }

  def checkOnlyDouble(p: String): Boolean = {
    val groups = p.toList.groupBy(_.toInt)
    groups.values.exists(_.size == 2)
  }
}
