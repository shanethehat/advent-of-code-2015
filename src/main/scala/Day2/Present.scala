package Day2

case class Present(w: Int, h: Int, l: Int) {

  private val shortestDimensions = Seq(w, h, l).sortWith((a, b) => a < b).slice(0, 2)

  val smallestSideArea: Int = shortestDimensions.product

  val smallestSidePerimeter: Int = shortestDimensions.sum * 2

  val area: Int = 2*(w * l) + 2*(w * h) + 2*(h * l)

  val volume: Int = w * h * l
}
