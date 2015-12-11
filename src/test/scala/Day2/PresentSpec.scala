package Day2

import org.scalatest._

class PresentSpec extends FlatSpec with Matchers {

  "A present" should "return the product of the 2 smallest sides" in {
    val present1 = Present(2, 3, 4)
    val present2 = Present(4, 3, 2)
    val present3 = Present(3, 2, 4)

    present1.smallestSideArea should be(6)
    present2.smallestSideArea should be(6)
    present3.smallestSideArea should be(6)
  }

  "A present" should "return its area" in {
    val present = Present(2, 3, 4)
    present.area should be(52)
  }
}
