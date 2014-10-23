package eu.swdev.xml.base

sealed trait Ternary {
  def ||(ternary: =>Ternary): Ternary
  def &&(ternary: =>Ternary): Ternary
  def unary_!(): Ternary
  def isNotFalse: Boolean
  def isNotTrue: Boolean
}

object True extends Ternary {
  def ||(ternary: =>Ternary) = True
  def &&(ternary: =>Ternary) = ternary match {
	  case True => True
	  case False => False
	  case Unknown => Unknown
  }
  def unary_!() = False
  def isNotFalse = true
  def isNotTrue = false
}

object False extends Ternary {
  def ||(ternary: =>Ternary) = ternary match {
	  case True => True
	  case False => False
	  case Unknown => Unknown
  }
  def &&(ternary: =>Ternary) = False
  def unary_!() = True
  def isNotFalse = false
  def isNotTrue = true
}

object Unknown extends Ternary {
  def ||(ternary: =>Ternary) = ternary match {
	  case True => True
	  case _ => Unknown
  }
  def &&(ternary: =>Ternary) = ternary match {
	  case False => False
	  case _ => Unknown
  }
  def unary_!() = Unknown
  def isNotFalse = true
  def isNotTrue = true
}