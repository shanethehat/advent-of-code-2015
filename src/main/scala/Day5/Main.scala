package Day5

import scala.io.Source

object Main extends App {
  val lines = Source.fromFile("data/day5.txt").getLines().toList

  def containsEnoughVowels(s: String): Boolean = s.filter("aeiou".contains(_)).length >2

  def containsSequentialCharacter(s: String, c: Char): Boolean =
    if (s.isEmpty) false
    else if (s.head == c) true
    else containsSequentialCharacter(s.tail, s.head)

  def containsBadCharacters(s: String, bad: List[String]) = s.sliding(2).exists(p => bad.contains(p))

  def containsRepeatWithGap(s: String): Boolean = containsRepeatWithGap(s, 0, 0)
  def containsRepeatWithGap(s: String, a: Char, b: Char): Boolean =
    if (s.isEmpty) false
    else if (s.head == a) true
    else containsRepeatWithGap(s.tail, b, s.head)

  def containsRepeatingPair(s: String):Boolean = {
    if (s.isEmpty) false
    else {
      val (term, target) = s.splitAt(2)
      if (target.contains(term)) true
      else containsRepeatingPair(s.tail)
    }
  }

  def isNice1(s: String) = !containsBadCharacters(s, List("ab", "cd", "pq", "xy")) &&
    containsEnoughVowels(s) &&
    containsSequentialCharacter(s.tail, s.head)

  def isNice2(s: String) = containsRepeatWithGap(s) && containsRepeatingPair(s)

  val niceLines1Count = lines.count(isNice1)
  val niceLines2Count = lines.count(isNice2)

  println(s"There are $niceLines1Count nice lines using the first rules and $niceLines2Count nice lines using the second rules")
}
