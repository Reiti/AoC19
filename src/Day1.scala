import util.Util

object Day1 {
  def main(args: Array[String]): Unit = {

    val masses = Util.loadDay(1).split("\n").map(_.toInt)

    //Part 1
    println(masses.map(fuelCost).sum)

    //Part 2
    println(masses.map(fuelCostReal).sum)

  }

  def fuelCost(mass: Int): Int = mass/3 - 2

  def fuelCostReal(mass: Int): Int = {
    if(fuelCost(mass) <= 0) {
      return 0
    }
    fuelCost(mass) + fuelCostReal(fuelCost(mass))
  }
}
