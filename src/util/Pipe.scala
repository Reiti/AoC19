package util

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

class Pipe {
  val data: BlockingQueue[BigInt] = new LinkedBlockingQueue[BigInt]()

  def empty(): Boolean = data.isEmpty
  def size(): Int = data.size
  def write(v: BigInt): Unit = data.add(v)
  def read(): BigInt = data.take()
  def readLast(): BigInt = {
    while(data.size() > 1 ) {
      data.take()
    }
    data.take()
  }
}
