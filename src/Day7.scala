import util.{IntcodeVM, IntcodeVMSharedMemory, SharedMem, Util}

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

    val mem_a = new SharedMem
    val mem_b = new SharedMem
    val mem_c = new SharedMem
    val mem_d = new SharedMem
    val mem_e = new SharedMem

    val a_s = new IntcodeVMSharedMemory(program)
    val b_s = new IntcodeVMSharedMemory(program)
    val c_s = new IntcodeVMSharedMemory(program)
    val d_s = new IntcodeVMSharedMemory(program)
    val e_s = new IntcodeVMSharedMemory(program)
    implicit val ec: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))

    val res = ((5 to 9).permutations map {conf =>
      mem_e.write(conf(0))
      mem_e.write(0)
      mem_a.write(conf(1))
      mem_b.write(conf(2))
      mem_c.write(conf(3))
      mem_d.write(conf(4))

      val a_r = Future {a_s.run(mem_e, mem_a)}
      val b_r = Future {b_s.run(mem_a, mem_b)}
      val c_r = Future {c_s.run(mem_b, mem_c)}
      val d_r = Future {d_s.run(mem_c, mem_d)}
      val e_r = Future {e_s.run(mem_d, mem_e)}

      val out = Await.result(e_r, Duration.Inf)
      out.read()
    }).max


    //Part 2
    println(res)


  }
}


