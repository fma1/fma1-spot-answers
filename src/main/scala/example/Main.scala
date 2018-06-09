package example

import scala.collection.mutable.Map

object Main extends App {
  // (s: weather, t: therapyw) => theeraw
  // (s: good, t: odg) => oodg
  // Assumption: t contains all characters that appear in s
  def sortByString(s: String, t: String): String = {
    // Map/hashtable from Letter (a, b, c, etc.) to Number of times it appears in s
    val letterOccurrences: Map[Char, Int] = Map[Char, Int]()
    // So for t = odg,
    // letterRanking(0) = o
    // letterRanking(1) = d
    // letterRanking(2) = g
    val letterRanking: IndexedSeq[Char] = for (i <- 0 until t.length) yield t.charAt(i)

    // Set number of occurrences for each letter that appears in s
    s.foreach(char => {
      letterOccurrences.get(char) match {
        case Some(occurrences) => letterOccurrences.put(char, occurrences + 1)
        case None => letterOccurrences.put(char, 1)
      }
    })

    // Map each character in letterRanking (o, d, g, etc.) to
    // a string which is the character repeated by the number of times it appears in s
    val sortedResultSeq = 
      letterRanking.map(char => {
        letterOccurrences.get(char) match {
          case Some(occurrences) => char.toString() * occurrences
          case None => ""
        }
      })

    // Just combine the "array" (it's an IndexedSeq, sort of a wrapper class around native arrays)
    // of strings into one string
    sortedResultSeq.reduce((acc, curr) => acc + curr)
  }
}
