import util.{IntcodeVMSuspendable, Util, VMState}

import scala.annotation.tailrec

object Day23 extends App {
  val program = Util.loadDayProgram(23)
  val vm = new IntcodeVMSuspendable(program)

  val machines = (0 to 49).map(id => vm.run(List(id), List())).toList

  //Part 1
  println(run(machines, vm, Map()))

  //Part 2
  println(runWithNat(machines, vm, Map(), (-1, -1), Set()))


  @tailrec
  def run(machines: List[VMState], vm: IntcodeVMSuspendable, packets: Map[Long, List[Long]]): Long = {
    val newPacks = scala.collection.mutable.HashMap[Long, List[Long]]()
    var found = false
    var res = -1L
    newPacks.addAll(packets)

    val newStates = for ((state, idx) <- machines.zipWithIndex) yield {
      val input = newPacks.getOrElse(idx, List(-1L))
      newPacks.remove(idx)
      val s = vm.run(state, input, List())
      for (sent <- s.output.reverse.grouped(3)) {
        if (!found && sent.head == 255) {
          found = true
          res = sent(2)
        }
        newPacks.update(sent.head, newPacks.getOrElse(sent.head, List()) ++ List(sent(1), sent(2)))
      }
      s
    }
    if (!found) {
      run(newStates, vm, newPacks.toMap)
    } else {
      res
    }
  }


  @tailrec
  def runWithNat(machines: List[VMState], vm: IntcodeVMSuspendable, packets: Map[Long, List[Long]], nat: (Long, Long), occured: Set[Long]): Long = {
    val newPacks = scala.collection.mutable.HashMap[Long, List[Long]]()
    var found = false
    var newNat = nat
    newPacks.addAll(packets)

    val newStates = for((state, idx) <- machines.zipWithIndex) yield {
      val input = newPacks.getOrElse(idx, List(-1L))
      newPacks.remove(idx)
      val s = vm.run(state, input, List())
      for(sent <- s.output.reverse.grouped(3)) {
        if(sent.head == 255) {
          found = true
          newNat = (sent(1), sent(2))
        }
        else {
          newPacks.update(sent.head, newPacks.getOrElse(sent.head, List()) ++ List(sent(1), sent(2)))
        }
      }
      s
    }

    if(newPacks.values.forall(_.isEmpty)) {
      if(occured.contains(newNat._2)) {
        newNat._2
      }
      else {
        runWithNat(newStates, vm, Map(0L -> List(newNat._1, newNat._2)), newNat, occured union Set(nat._2))
      }
    }
    else {
      runWithNat(newStates, vm, newPacks.toMap, newNat, occured)
    }
  }
}
