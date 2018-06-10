package example

import org.scalatest._

import One.sortByString

class OneSpec extends FlatSpec with Matchers {
  "The sortByString function" should "return theeraw when given weather and therapyw" in {
    assert(sortByString("weather", "therapyw") == "theeraw")
  }

  it should "return oodg when given good and odg" in {
    assert(sortByString("good", "odg") == "oodg")
  }

  it should "return bccemruu when given cucumber and bcemru" in {
    assert(sortByString("cucumber", "bcemru") == "bccemruu")
  }

  it should "return ceehlloorst when given cholesterol and cehlorst" in {
    assert(sortByString("cholesterol", "cehlorst") == "ceehlloorst")
  }
}
