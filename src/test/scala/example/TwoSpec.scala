package example

import org.scalatest._

import Two.decodeString

class TwoSpec extends FlatSpec with Matchers {
  "The decodeString function" should "return abababab when given 4[ab]" in {
    assert(decodeString("4[ab]") == "abababab")
  }

  it should "return baaabaaa when given 2[b3[a]]" in {
    assert(decodeString("2[b3[a]]") == "baaabaaa")
  }

  it should "return bacggfffffrrracggfffffrrracggfffffrrrbacggfffffrrracggfffffrrracggfffffrrr when given 2[b3[ac2[g]5[f]3[r]]]" in {
    assert(decodeString("2[b3[ac2[g]5[f]3[r]]]") == "bacggfffffrrracggfffffrrracggfffffrrrbacggfffffrrracggfffffrrracggfffffrrr")
  }
}
