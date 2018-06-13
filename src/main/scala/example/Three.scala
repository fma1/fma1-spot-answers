package example

import scala.collection.mutable.{ListBuffer, Map => MutableMap, Set => MutableSet}
import scala.collection.immutable.Set

object Three {
  def changePossibilities(amount: Int, denominations: List[Int]): Set[List[Int]] = {
    val map: MutableMap[Int, MutableSet[List[Int]]] = MutableMap[Int, MutableSet[List[Int]]]()

    // Adds all amounts in amountAcc based on the denominations in denomAcc
    def addToMap(denomAcc: List[Int], amountAcc: List[Int]): Unit = {
      val zipLst = amountAcc.zipAll(denomAcc, 0, 0)
      val denomLst = ListBuffer[Int]()

      zipLst.foreach({ case (amount, denomination) => {
        denomLst.prepend(denomination)
        // If the amount is 0, the denominations are
        // just everything in denomAcc, or the denomination list
        if (amount == 0) {
          map.get(amountAcc.last) match {
            case Some(oldSet) =>
              oldSet.add(denomAcc)
            case None =>
              val newSet = MutableSet[List[Int]]()
              newSet.add(denomAcc)
              map.put(amount, newSet)
          }
        // But if the amount isn't 0 we're going to go by
        // the ListBuffer
        // Basically after each iteration of this foreach loop
        // the ListBuffer will get another denomination prepended
        // to it
        } else {
          map.get(amount) match {
            case Some(oldSet) =>
              oldSet.add(denomLst.toList)
            case None =>
              val newSet = MutableSet[List[Int]]()
              newSet.add(denomLst.toList)
              map.put(amount, newSet)
          }
        }
      }})
    }

    def changeHelper(amount: Int, denomAcc: List[Int], amountAcc: List[Int]): Unit = {
      // If amount is less than 0 this isn't a valid denomination
      if (amount < 0) {
        ()
      }
      // If the amount is in the Map already, just use the previously computed value
      else if (map.contains(amount)) {
        val prevDenomSet: MutableSet[List[Int]] = map.get(amount).get

        prevDenomSet.foreach(prevDenomLst => {
          val newDenomLst: List[Int] = denomAcc ++ prevDenomLst
          addToMap(newDenomLst, amountAcc)
        })
      }
      // If amount is 0 then add the amounts to the map
      else if (amount == 0) {
        addToMap(denomAcc, amountAcc)
      // Else go through the different possibilities
      } else {
        denominations.foreach(denomination => changeHelper(amount - denomination, denomination :: denomAcc, amount :: amountAcc))
      }
    }

    changeHelper(amount, List[Int](), List[Int]())

    map.get(amount) match {
      case Some(resultSet) => resultSet.toSet
      case None => Set[List[Int]]()
    }
  }
}
