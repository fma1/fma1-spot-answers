package example

import org.scalatest._

import Three.changePossibilities

class ThreeSpec extends FlatSpec with Matchers {
  "The changePossibilities function" should "return [1, 1, 1, 1], [1, 1, 2], [1, 3], [2, 2] when given amount = 4 and denominations = [1, 2, 3]" in {
    val result = changePossibilities(4, Array(1, 2, 3))
    assert(result.contains(Array(1, 1, 1, 1)))
    assert(result.contains(Array(1, 1, 2)))
    assert(result.contains(Array(1, 3)))
    assert(result.contains(Array(2, 2)))
  }
}
