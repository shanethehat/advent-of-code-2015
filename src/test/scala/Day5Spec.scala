import org.scalatest._
import Day5.Main._

class Day5Spec extends FlatSpec with Matchers {

  it should "allow strings containing 3 or more vowels" in {
    containsEnoughVowels("aei") should be(true)
    containsEnoughVowels("afesi") should be(true)
    containsEnoughVowels("azzzedsdasdaidsadsad") should be(true)
    containsEnoughVowels("aaa") should be(true)
    containsEnoughVowels("ab") should be(false)
  }

  it should "allow strings containing sequential characters" in {
    containsSequentialCharacter("bcdeffg", 'a') should be(true)
    containsSequentialCharacter("bcdeffg", 'b') should be(true)
    containsSequentialCharacter("bcdefgg", 'a') should be(true)
    containsSequentialCharacter("bcdefg", 'a') should be(false)
  }

  it should "detect strings containing illegal character sequences" in {
    containsBadCharacters("abcde", List("aa", "cd")) should be(true)
    containsBadCharacters("abcde", List("aa", "ce")) should be(false)
    containsBadCharacters("abcde", List("ae", "ea")) should be(false)
  }

  it should "Detect strings containing same characters with a single gap" in {
    containsRepeatWithGap("aba") should be(true)
    containsRepeatWithGap("gfdsaba") should be(true)
    containsRepeatWithGap("abcdefga") should be(false)
    containsRepeatWithGap("abcdefg") should be(false)
    containsRepeatWithGap("abbc") should be(false)
  }

  it should "detect a string where a pair repeats" in {
    containsRepeatingPair("abcab") should be(true)
    containsRepeatingPair("abcdefghab") should be(true)
    containsRepeatingPair("habcdefghab") should be(true)
  }

  it should "reject a string where the repeating pair overlaps" in {
    containsRepeatingPair("aaa") should be(false)
  }
}
