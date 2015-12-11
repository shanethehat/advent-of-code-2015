import org.scalatest._
import Day3.Main._

class Day3Spec extends FlatSpec with Matchers {

  it should "modify a house when given an up arrow" in {
    getHouseFromDirection('^', House(0, 0)) should be(House(0, 1))
  }

  it should "modify a house when given a left arrow" in {
    getHouseFromDirection('<', House(0, 0)) should be(House(-1, 0))
  }

  it should "modify a house when given a right arrow" in {
    getHouseFromDirection('>', House(0, 0)) should be(House(1, 0))
  }

  it should "modify a house when given a down arrow" in {
    getHouseFromDirection('v', House(0, 0)) should be(House(0, -1))
  }
}
