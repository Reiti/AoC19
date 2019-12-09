package util

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

class Pipe {
  val data: BlockingQueue[Long] = new LinkedBlockingQueue[Long]()

  def empty(): Boolean = data.isEmpty
  def size(): Int = data.size
  def write(v: Long): Unit = data.add(v)
  def read(): Long = data.take()
  def readLast(): Long = {
    while(data.size() > 1 ) {
      data.take()
    }
    data.take()
  }
}
