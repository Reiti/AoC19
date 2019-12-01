import util.Util

object Day1 {
  def main(args: Array[String]): Unit = {

    val masses = Util.loadDayInts(1)

    //Part 1
    println(masses.map(fuelCost).sum)

    //Part 2
    println(masses.map(fuelCostReal).sum)

  }

  def fuelCost(mass: Int): Int = mass/3 - 2

  def fuelCostReal(mass: Int): Int = {
    if(fuelCost(mass) <= 0) {
      0
    }
    else {
      fuelCost(mass) + fuelCostReal(fuelCost(mass))
    }
  }
}
