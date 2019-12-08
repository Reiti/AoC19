import util.Util

object Day8 {
  def main(args: Array[String]): Unit = {
    val w = 25
    val h = 6
    val layers = Util.loadDay(8).strip().grouped(w*h).toList

    //Part 1
    println(layers.foldLeft((w*h, 0))({
      case ((zeroes, product), curr) =>
        val nZeroes = curr.count(_ == '0')
        val nProduct = curr.count(_ == '1') * curr.count(_ == '2')

        if(nZeroes < zeroes) {
          (nZeroes, nProduct)
        }
        else {
          (zeroes, product)
        }
    })._2)

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
