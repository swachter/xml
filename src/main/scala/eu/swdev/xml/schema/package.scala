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

  val shortType = ShortType(SHORT, intType, Facets.withWspCollapse)

  val byteType = ByteType(BYTE, shortType, Facets.withWspCollapse)

  val stringType = StringType(STRING, anyAtomicType, Facets.withWspPreserve)

  val qNameType = QNameType(QNAME, anyAtomicType, Facets.withWspCollapse)

  //

  val nonNegativeIntegerType = IntegerType(NON_NEGATIVE_INTEGER, integerType, Facets.withWspCollapse[AtomicVal[BigInt]].minInc.checkAndSet(AtomicVal(integerType, BigInt(0))).right.get)

  val positiveIntegerType = IntegerType(POSITIVE_INTEGER, nonNegativeIntegerType, Facets.withWspCollapse[AtomicVal[BigInt]].minExc.checkAndSet(AtomicVal(integerType, BigInt(0))).right.get)

  val unsignedLongType = IntegerType(UNSIGNED_LONG, nonNegativeIntegerType, Facets.withWspCollapse[AtomicVal[BigInt]].minInc.checkAndSet(AtomicVal(integerType, BigInt(0))).right.get.maxInc.checkAndSet(AtomicVal(integerType, BigInt("18446744073709551615"))).right.get)

  val unsignedIntType = LongType(UNSIGNED_INT, unsignedLongType, Facets.withWspCollapse[AtomicVal[Long]].minInc.checkAndSet(AtomicVal(longType, 0)).right.get.maxInc.checkAndSet(AtomicVal(longType, 4294967295l)).right.get)

  val unsignedShortType = IntType(UNSIGNED_SHORT, unsignedIntType, Facets.withWspCollapse[AtomicVal[Int]].minInc.checkAndSet(AtomicVal(intType, 0)).right.get.maxInc.checkAndSet(AtomicVal(intType, 65535)).right.get)

  val unsignedByteType = ShortType(UNSIGNED_BYTE, unsignedShortType, Facets.withWspCollapse[AtomicVal[Short]].minInc.checkAndSet(AtomicVal(shortType, 0)).right.get.maxInc.checkAndSet(AtomicVal(shortType, 255)).right.get)

  //

  val nonPositiveIntegerType = IntegerType(NON_POSITIVE_INTEGER, integerType, Facets.withWspCollapse[AtomicVal[BigInt]].maxInc.checkAndSet(AtomicVal(integerType, BigInt(0))).right.get)

  val negativeIntegerType = IntegerType(NEGATIVE_INTEGER, nonPositiveIntegerType, Facets.withWspCollapse[AtomicVal[BigInt]].maxExc.checkAndSet(AtomicVal(integerType, BigInt(0))).right.get)

  //

  val normalizedStringType = StringType(NORMALIZED_STRING, stringType, Facets.withWspReplace)

  val tokenType = StringType(TOKEN, normalizedStringType, Facets.withWspCollapse)

  val languageType = StringType(LANGUAGE, tokenType, Facets.withWspCollapse[AtomicVal[String]].pattern.checkAndSet(Seq("[a-zA-Z]{1,8}(-[a-zA-Z0-9]{1,8})*".r)).right.get)

  val nameType = StringType(NAME, tokenType, Facets.withWspCollapse[AtomicVal[String]].pattern.checkAndSet(Seq("""[\p{javaLowerCase}\p{javaUpperCase}_:][-.\p{Digit}\p{javaLowerCase}\p{javaUpperCase}_:]*""".r)).right.get)

  val ncNameType = StringType(NC_NAME, nameType, Facets.withWspCollapse[AtomicVal[String]].pattern.checkAndSet(Seq("""[\p{javaLowerCase}\p{javaUpperCase}_][-.\p{Digit}\p{javaLowerCase}\p{javaUpperCase}_]*""".r)).right.get)

  val entityType = StringType(ENTITY, ncNameType, Facets.withWspCollapse[AtomicVal[String]].pattern.checkAndSet(Seq("""[\p{javaLowerCase}\p{javaUpperCase}_][-.\p{Digit}\p{javaLowerCase}\p{javaUpperCase}_]*""".r)).right.get)

  val idType = StringType(ID, ncNameType, Facets.withWspCollapse[AtomicVal[String]].pattern.checkAndSet(Seq("""[\p{javaLowerCase}\p{javaUpperCase}_][-.\p{Digit}\p{javaLowerCase}\p{javaUpperCase}_]*""".r)).right.get)

  val idRefType = StringType(IDREF, ncNameType, Facets.withWspCollapse[AtomicVal[String]].pattern.checkAndSet(Seq("""[\p{javaLowerCase}\p{javaUpperCase}_][-.\p{Digit}\p{javaLowerCase}\p{javaUpperCase}_]*""".r)).right.get)

  val nmTokenType = StringType(NAME, tokenType, Facets.withWspCollapse[AtomicVal[String]].pattern.checkAndSet(Seq("""[-.\p{Digit}\p{javaLowerCase}\p{javaUpperCase}_:]+""".r)).right.get)

  def unionVal[T <: AtomicOrListType](memberTypes: List[T])(lexicalRep: String, ns: Namespaces): Either[String, T#VAL] =
    memberTypes.iterator.map(at => Try { at.createVal(lexicalRep, ns) }).find(_.isSuccess).map(_.get).getOrElse(throw new IllegalArgumentException(s"invalid value: $lexicalRep; non of the union member types could parse the value"))

}
