package eu.swdev.xml.schema

import eu.swdev.xml.base.WhitespaceProcessing

/**
  */
case class WhitespaceFacet(whitespaceProcess: WhitespaceProcessing, fixed: Boolean)

object WhitespaceFacet {

  val COLLAPSE_FIXED = WhitespaceFacet(WhitespaceProcessing.Collapse, true)
  val PRESERVER_UNFIXED = WhitespaceFacet(WhitespaceProcessing.Preserve, false)
}
