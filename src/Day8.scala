import util.Util

object Day8 {
  def main(args: Array[String]): Unit = {
    val w = 25
    val h = 6
    val layers = Util.loadDay(8).strip().grouped(w*h).toList

    val minLayer = layers.minBy(layer => layer.count(_ == '0'))

    //Part 1
    println(minLayer.count(_ == '1') * minLayer.count(_ == '2'))

    //Part 2
    for(y <- 0 until h) {
      for(x <- 0 until w) {
        print(decodePixel(layers, x, y, w))
      }
      println()
    }
  }

  def decodePixel(layers: List[String], x: Int, y: Int, w: Int): Char = {
    layers.view.map(layer => layer(y*w + x)).find(p => p != '2').getOrElse(' ')
  }
}
