package eu.swdev.xml.base

/**
  */
sealed trait SomeValue[X] {
  def value: X
}

object SomeValue {
  def defined[X](x: X): SomeValue[X] = DefinedValue(x)
  def default[X](x: X): SomeValue[X] = DefaultValue(x)
}

case class DefinedValue[X](value: X) extends SomeValue[X]

case class DefaultValue[X](value: X) extends SomeValue[X]
