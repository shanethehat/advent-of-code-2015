package Day2

import scala.io.Source

object Main extends App {
  val lines = Source.fromFile("data/day2.txt").getLines()

  val presents = lines.map { l =>
    val ds =l.split("x").map {_.toInt}
    Present(ds{0}, ds{1}, ds{2})
  }

  case class PresentTotals(totalArea: Int, totalRibbon: Int)

  val totals = presents.foldLeft(PresentTotals(0, 0))((t, p) => {
    PresentTotals(
      t.totalArea + p.area + p.smallestSideArea,
      t.totalRibbon + p.smallestSidePerimeter + p.volume
    )
  })

  println(s"Area of wrapping paper: ${totals.totalArea}")
  println(s"Length of ribbon: ${totals.totalRibbon}")
}
