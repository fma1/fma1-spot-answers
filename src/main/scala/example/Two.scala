package example

object Two {
  def decodeString(str: String): String = {
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
