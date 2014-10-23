package eu.swdev.xml

/**
  */
package object xsd {

  import XsdNames._

  val booleanType = BooleanType(BOOLEAN, anyAtomicType)

  val doubleType = DoubleType(DOUBLE, anyAtomicType)

  val decimalType = DecimalType(DECIMAL, anyAtomicType)

  val integerType = IntegerType(INTEGER, decimalType)

  val longType = LongType(LONG, integerType)

  val intType = IntType(INT, longType)

  val stringType = StringType(STRING, anyAtomicType, WhitespaceFacet.PRESERVER_UNFIXED)

  val qNameType = QNameType(QNAME, anyAtomicType)

}
