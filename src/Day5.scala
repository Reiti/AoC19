import util.Util
import util.IntcodeVM_old1

object Day5 {
  def main(args: Array[String]): Unit = {
    val vm = new IntcodeVM_old1(Util.loadDay(5).split(",") map {_.toInt})

    //Part 1
    vm.run(List(1)).foreach(println)

    //Part 2
    vm.run(List(5)).foreach(println)
  }
}
