import util.{IntcodeVMSuspendable, Util}

object Day17 extends App {
  val program = Util.loadDayProgram(17)

  val vm = new IntcodeVMSuspendable(program)

  val map = vm.run(List(), List()).output.reverse.map(_.toChar).mkString.split("\n")
  /*val map = """..#..........
              |..#..........
              |#######...###
              |#.#...#...#.#
              |#############
              |..#...#...#..
              |..#####...^..""".stripMargin.strip.split("\n").map(_.strip)
*/
  val intersections = for {
    y <- map.indices
    x <- map(y).indices

    up = if(y > 0) map(y-1)(x) else '.'
    left = if(x > 0) map(y)(x-1) else '.'
    right = if(x < map(y).length - 1) map(y)(x+1) else '.'
    down = if(y < map.size - 1) map(y+1)(x) else '.'
    if up == '#' && down == '#' && left == '#' && right == '#' && map(y)(x) == '#'
  } yield (x, y)

  //Part 1
  println(intersections.map(x => x._1*x._2).sum)


  val move = List('A',',','B',',','A',',','B',',','C',',','B',',','A',',','C',',','B',',','C', '\n').map(_.toLong)
  val a = List('L',',','1','2',',','L',',','8',',','R',',','1','0',',','R',',','1','0', '\n').map(_.toLong)
  val b = List('L',',','6',',','L',',','4',',','L',',','1','2', '\n').map(_.toLong)
  val c = List('R',',','1','0',',','L',',','8',',','L',',','4',',','R',',','1','0','\n').map(_.toLong)


  val newVm = new IntcodeVMSuspendable(program.updated(0, 2))
  val state1 = newVm.run(move, List())
  val state2 = newVm.run(state1, a, List())
  val state3 = newVm.run(state2, b, List())
  val state4 = newVm.run(state3, c, List())
  val result = newVm.run(state4, List('n'.toLong, '\n'.toLong), List())

  //Part 2
  println(result.output.head)



}
