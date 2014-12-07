package eu.swdev.xml

import eu.swdev.xml.name.Namespaces

import scala.util.Try

/**
  */
package object schema {

  import XsNames._

  val booleanType = BooleanType(BOOLEAN, anyAtomicType)

  val doubleType = DoubleType(DOUBLE, anyAtomicType)

  val decimalType = DecimalType(DECIMAL, anyAtomicType)

  val integerType = IntegerType(INTEGER, decimalType)

  val longType = LongType(LONG, integerType)

  val intType = IntType(INT, longType)

  val stringType = StringType(STRING, anyAtomicType, WhitespaceFacet.PRESERVE_UNFIXED)

  val qNameType = QNameType(QNAME, anyAtomicType)

  def unionVal[T <: AtomicOrListType](memberTypes: List[T])(lexicalRep: String, ns: Namespaces): Either[String, T#VAL] =
    memberTypes.iterator.map(at => Try { at.createVal(lexicalRep, ns) }).find(_.isSuccess).map(_.get).getOrElse(throw new IllegalArgumentException(s"invalid value: $lexicalRep; non of the union member types could parse the value"))

}
