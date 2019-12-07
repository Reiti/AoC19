package util

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

class SharedMem {
  val data: BlockingQueue[Int] = new LinkedBlockingQueue[Int]()

  def empty(): Boolean = data.isEmpty
  def size(): Int = data.size
  def write(v: Int): Unit = data.add(v)
  def read(): Int = data.take()
}
