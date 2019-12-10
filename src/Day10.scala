import util.Util

object Day10 extends App {
  val input = Util.loadDay(10).split("\n").map(_.trim)

  val asteroids = for {
    x <- input(0).indices
    y <- input.indices
    if input(y)(x) == '#'
  } yield (x, y)

  val polars = asteroids.map(a => {
    (a, asteroids.map(b => polar(a, b)))
  })

  val best = polars.map(a => (a._1, a._2.distinctBy(_._1).size)).maxBy(_._2)

  //Part 1
  println(best._2)

  val bestPolars = polars.find(_._1 == best._1).get._2.filter(_ != (0, 0)).sortBy(_._2)
  val grouped = bestPolars.groupBy(_._1).toSeq.sortBy(_._1)
  val fromStart = grouped.dropWhile(_._1 < 90) ++ grouped.takeWhile(_._1 < 90)

  val maxLength = fromStart.map(_._2.size).max
  val res = fromStart.map(_._2.padTo(maxLength, (0.0, 0.0))).transpose.flatten.drop(199).head

  //Part 2
  println(asteroids.find(a => polar(best._1, a) == res).get)

  def polar(base: (Int, Int), coords: (Int, Int)): (Double, Double) = {
    val xr = base._1 - coords._1
    val yr = base._2 - coords._2
    ((Math.toDegrees(Math.atan2(yr, xr)) + 360) % 360, Math.sqrt(xr * xr + yr * yr))
  }
}
