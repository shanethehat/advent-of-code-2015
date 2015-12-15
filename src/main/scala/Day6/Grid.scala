package Day6

import scala.annotation.tailrec

case class Point(x: Int, y: Int)

object Grid {
  def createEmpty(size: Int): List[List[Int]] = {
    List.fill(size, size)(0)
  }

  def createFromBinaryCommand(c: Command, g: List[List[Int]]): List[List[Int]] = c match {
    case OnCommand(s, e) => updateYAxis(s, e, 0, g, _ => 1)
    case OffCommand(s, e) => updateYAxis(s, e, 0, g, _ => 0)
    case ToggleCommand(s, e) =>updateYAxis(s, e, 0, g, a => if (a == 1) 0 else 1)
  }

  def createFromIncrementalCommand(c: Command, g: List[List[Int]]): List[List[Int]] = c match {
    case OnCommand(s, e) => updateYAxis(s, e, 0, g, a => a + 1)
    case OffCommand(s, e) => updateYAxis(s, e, 0, g, a => if (a > 0) a - 1 else 0)
    case ToggleCommand(s, e) =>updateYAxis(s, e, 0, g, a => a + 2)
  }

  @tailrec
  private def updateYAxis(s: Point, e: Point, p: Int, g: List[List[Int]], f: Int =>Int): List[List[Int]] =
    if (p > e.y) g
    else if (p < s.y) updateYAxis(s, e, p + 1, g, f)
    else updateYAxis(s, e, p + 1, updateXAxis(s, e, g, f, p), f)

  private def updateXAxis(s: Point, e: Point, g: List[List[Int]], f: Int =>Int, p:Int): List[List[Int]] = {
    val newRow: List[Int] = g(p).patch(s.x, g(p).slice(s.x, e.x + 1).map(f), e.x - s.x + 1)
    g.patch(p, Seq(newRow), 1)
  }
}
