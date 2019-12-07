package util

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Util {
  def loadDay(day: Int): String = {
    Using(Source.fromResource(s"$day.txt")) {
      source => source.mkString.strip()
    }.get
  }

  def loadDayLines(day: Int): List[String] = loadDay(day).split("\n").toList

  def loadDayInts(day: Int): List[Int] = loadDayLines(day).map(_.toInt)

}

class SharedMem {
  var data: mutable.Queue[Int] = mutable.Queue()

  def empty(): Boolean = data.isEmpty
  def size(): Int = data.size
  def write(v: Int): Unit = {
    data.enqueue(v)
  }
  def read(): Option[Int] = {
    if(data.isEmpty) {
      None
    }
    else {
      val x = data.dequeue()
      Some(x)
    }
  }
}
