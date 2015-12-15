package Day6

import scala.io.Source

object Main extends App {
  val lines = Source.fromFile("data/day6.txt").getLines().toList

  def runCommands(f: (Command, List[List[Int]]) => List[List[Int]], grid: List[List[Int]], lines: List[String]): List[List[Int]] =
    if (lines.isEmpty) grid
    else runCommands(f, f(Command.createFromString(lines.head), grid), lines.tail)

  val initialGrid = Grid.createEmpty(1000)
  val finalBinaryGrid = runCommands(Grid.createFromBinaryCommand, initialGrid, lines)
  val finalIncrementalGrid = runCommands(Grid.createFromIncrementalCommand, initialGrid, lines)

  val totalLightsOn = finalBinaryGrid.flatten.sum
  val totalBrightness = finalIncrementalGrid.flatten.sum

  println(s"There are $totalLightsOn lights on")
  println(s"The total brightness is $totalBrightness")
}
