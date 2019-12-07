package util

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

class Pipe {
  val data: BlockingQueue[Int] = new LinkedBlockingQueue[Int]()

  def empty(): Boolean = data.isEmpty
  def size(): Int = data.size
  def write(v: Int): Unit = data.add(v)
  def read(): Int = data.take()
  def readLast(): Int = {
    while(data.size() > 1 ) {
      data.take()
    }
    data.take()
  }
}
