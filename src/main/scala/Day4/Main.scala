package Day4

import java.security.MessageDigest

object Main extends App {
  val prefix = "iwrupvqb"

  def makeHash(s: String) = MessageDigest.getInstance("MD5").digest(s.getBytes).map("%02X".format(_)).mkString

  def findFirstHashMatching(start: String, count: Int): Int =
    if (makeHash(prefix + count.toInt).startsWith(start)) count
    else findFirstHashMatching(start, count + 1)

  val firstHash = findFirstHashMatching("00000", 1)
  val secondHash = findFirstHashMatching("000000", firstHash)

  println("The lowest number to create a MD5 hash with 5 leading zeroes is: " + firstHash)
  println("The lowest number to create a MD5 hash with 6 leading zeroes is: " + secondHash)
}
