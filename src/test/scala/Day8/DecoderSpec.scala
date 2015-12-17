package Day8

import org.scalatest._
import Day8.Processor.Decoder

class DecoderSpec extends FlatSpec with Matchers {

  it should "allow unescaped characters" in {
    Decoder.process("abcdefg") should be("abcdefg")
  }

  it should "ignore unescaped speech marks" in {
    Decoder.process("""ab"c""") should be("abc")
  }

  it should "treat an escaped backslash as a backslash" in {
    Decoder.process("""ab\\c""") should be("""ab\c""")
  }

  it should "preserve an escaped speechmark" in {
    Decoder.process("""ab\"c""") should be("""ab"c""")
  }

  it should "translate a ascii sequence" in {
    Decoder.process("""ab\x27c""") should be("ab'c")
  }

  it should "remove control characers" in {
    Decoder.process("ab^c") should be("abc")
  }
}
