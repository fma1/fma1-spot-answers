package example

import scala.collection.mutable.{ListBuffer, Map => MutableMap, Set => MutableSet}
import scala.collection.immutable.Set

object Three {
  def changePossibilities(amount: Int, denominations: List[Int]): Set[List[Int]] = {
    val map: MutableMap[Int, MutableSet[List[Int]]] = MutableMap[Int, MutableSet[List[Int]]]()

    def addToMap(denomAcc: List[Int], amountAcc: List[Int]): Unit = {
      val zipLst = amountAcc.zipAll(denomAcc, 0, 0)
      val denomLst = ListBuffer[Int]()

      zipLst.foreach({ case (amount, denomination) => {
        denomLst.prepend(denomination)
        if (amount != 0) {
          map.get(amount) match {
            case Some(oldSet) =>
              oldSet.add(denomLst.toList)
            case None =>
              val newSet = MutableSet[List[Int]]()
              newSet.add(denomLst.toList)
              map.put(amount, newSet)
          }
        } else if (amount == 0) {
          map.get(amountAcc.last) match {
            case Some(oldSet) =>
              oldSet.add(denomAcc)
            case None =>
              val newSet = MutableSet[List[Int]]()
              newSet.add(denomAcc)
              map.put(amount, newSet)
          }
        }
      }})
    }

    def changeHelper(amount: Int, denomAcc: List[Int], amountAcc: List[Int]): Unit = {
      if (amount < 0) {
        ()
      }
      else if (map.contains(amount)) {
        val prevDenomSet: MutableSet[List[Int]] = map.get(amount).get

        prevDenomSet.foreach(prevDenomLst => {
          val newDenomLst: List[Int] = denomAcc ++ prevDenomLst
          addToMap(newDenomLst, amountAcc)
        })
      }
      else if (amount == 0) {
        addToMap(denomAcc, amountAcc)
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
