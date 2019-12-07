import util.{IntcodeVM, IntcodeVMPipes, Pipe, Util}

import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutorService, Future}
import java.util.concurrent.Executors

import scala.concurrent.duration.Duration

object Day7 {
  def main(args: Array[String]): Unit = {
    val program = Util.loadDay(7).split(",") map {_.toInt}

    val va = new IntcodeVM(program)
    val vb = new IntcodeVM(program)
    val vc = new IntcodeVM(program)
    val vd = new IntcodeVM(program)
    val ve = new IntcodeVM(program)

    //Part1
    println(((0 to 4).permutations map {conf =>
      ve.run(
        List(conf(4)) ++
        vd.run(
          List(conf(3)) ++
          vc.run(
            List(conf(2)) ++
            vb.run(
              List(conf(1)) ++
              va.run(
                List(conf(0)) ++
                List(0)
              )
            )
          )
        )
      ).head
    }).max)

    val mem_a = new Pipe
    val mem_b = new Pipe
    val mem_c = new Pipe
    val mem_d = new Pipe
    val mem_e = new Pipe

    val a_s = new IntcodeVMPipes(program)
    val b_s = new IntcodeVMPipes(program)
    val c_s = new IntcodeVMPipes(program)
    val d_s = new IntcodeVMPipes(program)
    val e_s = new IntcodeVMPipes(program)

    implicit val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))

    //Part 2
    println(((5 to 9).permutations map {conf =>
      mem_e.write(conf(0))
      mem_e.write(0)
      mem_a.write(conf(1))
      mem_b.write(conf(2))
      mem_c.write(conf(3))
      mem_d.write(conf(4))

      Future {a_s.run(mem_e, mem_a)}
      Future {b_s.run(mem_a, mem_b)}
      Future {c_s.run(mem_b, mem_c)}
      Future {d_s.run(mem_c, mem_d)}
      Await.result(Future {e_s.run(mem_d, mem_e)}, Duration.Inf).read()
    }).max)
  }
}


