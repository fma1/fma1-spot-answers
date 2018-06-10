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

  def decodeStr(str: String): String = {
    // Assumption: String is not malformed (i.e. string conforms to the context-free grammar given)
    def decodeStrHelper(str: String): (String, Int) = {
      val tmpBuf = new StringBuilder()
      var curIdx = 0 

      // Iterate through the string
      while (curIdx < str.length && str.charAt(curIdx) != ']') {
        // If the character at the current index is a digit
        // we get the location of the next opening bracket
        // and the number of times to repeat the encoded string
        // We recurse with str starting at the location of
        // the next opening bracket + 1
        // Then we just multiply the resulting string by the repeat number
        // add that to tmpBuf
        // and start after the index where it found the ending bracket
        if (str.charAt(curIdx).isDigit) {
          val curOpenBracket: Int = str.indexOf('[', curIdx)
          val curRepeatNum: Int = str.substring(curIdx, curOpenBracket).toInt

          decodeStrHelper(str.substring(curOpenBracket + 1)) match {
            case (resultStr, endIdx) =>
              tmpBuf.append(resultStr * curRepeatNum)
              curIdx = curOpenBracket + 1 + endIdx + 1
          }
        // If it's not a digit that means we just have a string
        // so we figure out where the string ends
        // and add the string to tmpBuf
        } else {
          val nextIdx = str.indexWhere((char: Char) => !char.isLetter, curIdx)
          tmpBuf.append(str.substring(curIdx, nextIdx))
          curIdx = nextIdx
        }
      }

      (tmpBuf.toString(), curIdx)
    }

    decodeStrHelper(str)._1
  }
}
