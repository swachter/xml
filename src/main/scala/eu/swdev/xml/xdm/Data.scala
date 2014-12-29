package eu.swdev.xml.xdm

import eu.swdev.xml.name.QName

/**
 *
 */
sealed trait Item

sealed trait Node extends Item

sealed trait Function extends Item {

}

sealed trait Value {
  def typeName: QName
}

sealed trait SimpleValue extends Value

sealed trait AtomicValue extends SimpleValue with Item {
  type Data
  def data: Data
}

sealed trait ComplexValue extends Value {
  def attrs: Attrs
  def content: Content
}

sealed trait Content

sealed trait ComplexContent {
  def children: Children
}

sealed trait SimpleContent {
  def value: SimpleValue
}

//
//
//

sealed case class ListValue(typeName: QName, items: List[AtomicValue]) extends SimpleValue

sealed case class BooleanValue(typeName: QName, data: Boolean) extends AtomicValue {
  type Data = Boolean
}

sealed case class UntypedAtomicValue(typeName: QName, data: String) extends AtomicValue {
  type Data = String
}

sealed case class DoubleValue(typeName: QName, data: Double) extends AtomicValue {
  type Data = Double
}

sealed case class DecimalValue(typeName: QName, data: BigDecimal) extends AtomicValue {
  type Data = BigDecimal
}

sealed case class IntegerValue(typeName: QName, data: BigInt) extends AtomicValue {
  type Data = BigInt
}

sealed case class LongValue(typeName: QName, data: Long) extends AtomicValue {
  type Data = Long
}

sealed case class IntValue(typeName: QName, data: Int) extends AtomicValue {
  type Data = Int
}

sealed case class ShortValue(typeName: QName, data: Short) extends AtomicValue {
  type Data = Short
}

sealed case class ByteValue(typeName: QName, data: Byte) extends AtomicValue {
  type Data = Byte
}

sealed case class StringValue(typeName: QName, data: String) extends AtomicValue {
  type Data = String
}

sealed case class QNameValue(typeName: QName, data: QName) extends AtomicValue {
  type Data = QName
}



trait Attrs

trait Children