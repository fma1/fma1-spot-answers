package example

import org.scalatest._

class MainSpec extends FlatSpec with Matchers {
  "The sortByString function" should "return theeraw when given weather and therapyw" in {
    assert(Main.sortByString("weather", "therapyw") == "theeraw")
  }

  it should "return oodg when given good and odg" in {
    assert(Main.sortByString("good", "odg") == "oodg")
  }

  it should "return bccemruu when given cucumber and bcemru" in {
    assert(Main.sortByString("cucumber", "bcemru") == "bccemruu")
  }

  it should "return ceehlloorst when given cholesterol and cehlorst" in {
    assert(Main.sortByString("cholesterol", "cehlorst") == "ceehlloorst")
  }
}
