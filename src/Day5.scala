import util.Util
import util.IntcodeProgram

object Day5 {
  def main(args: Array[String]): Unit = {
    val program = new IntcodeProgram(Util.loadDay(5).split(",") map {_.toInt})

    //Part 1
    program.run(List(1))

    //Part 2
    program.run(List(5))
  }

}
