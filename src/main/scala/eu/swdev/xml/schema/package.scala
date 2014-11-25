package eu.swdev.xml

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

}
