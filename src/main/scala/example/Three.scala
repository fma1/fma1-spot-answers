package example

import scala.collection.mutable.{Map, Set => MutableSet}
import scala.collection.immutable.Set

object Three {
  // Sort of like a global hashtable for all the possible denominations
  // a certain denomination will map to another hashtable which maps
  // Int -> List[Int]
  lazy val maps = Map[List[Int], Map[Int, List[Int]]]()

  def changePossibilities(amount: Int, denominations: List[Int]): MutableSet[List[Int]] = {
    if (maps.get(denominations).isEmpty) {
      maps.put(denominations, Map[Int, List[Int]]())
    }

    val mutableSet: MutableSet[List[Int]] = MutableSet[List[Int]]()

    def changeHelper(amount: Int, denominations: List[Int], acc: List[Int]): Unit = {
      if (amount >= 0) {
        if (amount == 0) {
          set.add(acc)
        } else {
          denominations.foreach(denomination => changeHelper(amount - denomination, denominations, denomination :: acc))
          ()
        }
      }
    }

    changeHelper(amount, denominations, List[Int]())

    mutableSet
  }
}
