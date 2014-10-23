package eu.swdev.xml.base

/**
 * 
 */
sealed trait WhitespaceProcessing {
  def name: String
  /**
   * Checks if the specified whitespace processing method is the same whitespace
   * as this whitespace processing method or a restriction of it.
   */
  def isSameOrRestriction(wp: WhitespaceProcessing): Boolean
  def process(string: String): String
}

object WhitespaceProcessing {

  def isWhitespace(c: Char): Boolean = c == ' ' || c == '\n' || c == '\r' || c == '\t'

  case object Preserve extends WhitespaceProcessing {
    def name = "preserve"
    def isSameOrRestriction(wp: WhitespaceProcessing) = true
    def process(string: String) = string
  }

  case object Replace extends WhitespaceProcessing {
    def name = "replace"
    def isSameOrRestriction(wp: WhitespaceProcessing) = wp ne Preserve
    def process(string: String) = string.map((c) => if (isWhitespace(c)) ' ' else c)
  }

  case object Collapse extends WhitespaceProcessing {
    def name = "collapse"
    def isSameOrRestriction(wp: WhitespaceProcessing) = wp eq Collapse
    def process(string: String) = {
      // the string is processed by a state machine with the following states:
      // 1: initial state; on space -> ignore space; on non-space -> output non-space, change to state 2
      // 2: in word; on space -> change to state 3; on non-space output non-space
      // 3: after word; on space -> ignore space; on non-space -> output space and non-space, change to state 2
      val sb = new StringBuilder(string.length)
      var state = 1
      string.foreach((c) => {
        val isWs = isWhitespace(c)
        state match {
          case 1 =>
            if (!isWs) {
              sb.append(c)
              state = 2
            }
          case 2 =>
            if (isWs) {
              state = 3
            } else {
              sb.append(c)
            }
          case 3 =>
            if (!isWs) {
              sb.append(' ')
              sb.append(c)
              state = 2
            }
          }
        }
      )
      sb.toString
    }
  }

}