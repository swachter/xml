package eu.swdev.xml

import eu.swdev.xml.name.Namespaces

import scala.util.Try

/**
  */
package object schema {

  import XsNames._

  val booleanType = BooleanType(BOOLEAN, anyAtomicType, Facets.withWspCollapse)

  val doubleType = DoubleType(DOUBLE, anyAtomicType, Facets.withWspCollapse)

  val decimalType = DecimalType(DECIMAL, anyAtomicType, Facets.withWspCollapse)

  val integerType = IntegerType(INTEGER, decimalType, Facets.withWspCollapse)

  val longType = LongType(LONG, integerType, Facets.withWspCollapse)

  val intType = IntType(INT, longType, Facets.withWspCollapse)

  val stringType = StringType(STRING, anyAtomicType, Facets.withWspPreserve)

  val qNameType = QNameType(QNAME, anyAtomicType, Facets.withWspCollapse)

  def unionVal[T <: AtomicOrListType](memberTypes: List[T])(lexicalRep: String, ns: Namespaces): Either[String, T#VAL] =
    memberTypes.iterator.map(at => Try { at.createVal(lexicalRep, ns) }).find(_.isSuccess).map(_.get).getOrElse(throw new IllegalArgumentException(s"invalid value: $lexicalRep; non of the union member types could parse the value"))

}
