package Day8

import Day8.Processor.Encoder
import org.scalatest._

class EncoderSpec extends FlatSpec with Matchers {

  it should "wrap a given string in speech marks" in {
    Encoder.process("abc") should be(""""abc"""")
  }

  it should "encode a speech mark in a string" in {
    Encoder.process("""ab"c""") should be(""""ab\"c"""")
  }

  it should "encode a backslash and a speech mark in a string" in {
    Encoder.process("""aaa\"aaa""") should be(""""aaa\\\"aaa"""")
  }

  it should "add speechmarks when a control character is found" in {
    Encoder.process("abc^abc") should be(""""abc""abc"""")
  }
}
