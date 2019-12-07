package util

import scala.collection.mutable
import scala.io.Source
import scala.util.Using
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}

object Util {
  def loadDay(day: Int): String = {
    Using(Source.fromResource(s"$day.txt")) {
      source => source.mkString.strip()
    }.get
  }

  def loadDayLines(day: Int): List[String] = loadDay(day).split("\n").toList

  def loadDayInts(day: Int): List[Int] = loadDayLines(day).map(_.toInt)

}
