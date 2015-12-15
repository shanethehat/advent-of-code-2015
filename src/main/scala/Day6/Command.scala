package Day6

import scala.util.matching.Regex

sealed trait Command {
  def start: Point
  def end: Point
}

object Command {
  val regex: Regex = """([\w|\s]*) (\d{1,3}),(\d{1,3}) through (\d{1,3}),(\d{1,3})""".r

  def createFromString(string: String): Command = {
    val Some(m) = regex.findFirstMatchIn(string)

    val start: Point = Point(m.group(2).toInt, m.group(3).toInt)
    val end: Point = Point(m.group(4).toInt, m.group(5).toInt)

    m.group(1).trim match {
      case "toggle" => ToggleCommand(start, end)
      case "turn on"  => OnCommand(start, end)
      case "turn off" => OffCommand(start, end)
    }
  }
}

case class OnCommand(start: Point, end: Point) extends Command

case class OffCommand(start: Point, end: Point) extends Command

case class ToggleCommand(start: Point, end: Point) extends Command

