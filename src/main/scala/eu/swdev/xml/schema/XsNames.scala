package eu.swdev.xml.schema

import javax.xml.XMLConstants

import eu.swdev.xml.name.{LocalName, Namespace, Prefix, QNameFactory}

/**
  */
object XsNames {

  val XS_NAMESPACE = Namespace(XMLConstants.W3C_XML_SCHEMA_NS_URI)
  val XS_PREFIX = Prefix("xs")

  private def xsTypeName(localName: String) = QNameFactory.caching(XS_NAMESPACE, LocalName(localName), XS_PREFIX)

  val ANY_TYPE = xsTypeName("anyType")
  val ANY_SIMPLE_TYPE = xsTypeName("anySimpleType")
  val ANY_ATOMIC_TYPE = xsTypeName("anyAtomicType")

  val UNTYPED = xsTypeName("untyped")
  val UNTYPED_ATOMIC = xsTypeName("untypedAtomic")

  val BOOLEAN = xsTypeName("boolean")

  val DOUBLE = xsTypeName("double")
  val FLOAT = xsTypeName("float")

  val DECIMAL = xsTypeName("decimal")
  val INTEGER = xsTypeName("integer")
  val LONG = xsTypeName("long")
  val INT = xsTypeName("int")
  val SHORT = xsTypeName("short")
  val BYTE = xsTypeName("byte")

  val NEGATIVE_INTEGER = xsTypeName("negativeInteger")
  val POSITIVE_INTEGER = xsTypeName("positiveInteger")

  val NON_NEGATIVE_INTEGER = xsTypeName("nonNegativeInteger")
  val NON_POSITIVE_INTEGER = xsTypeName("nonPositiveInteger")

  val UNSIGNED_LONG = xsTypeName("unsignedLong")
  val UNSIGNED_INT = xsTypeName("unsignedInt")
  val UNSIGNED_SHORT = xsTypeName("unsignedShort")
  val UNSIGNED_BYTE = xsTypeName("unsignedByte")

  val STRING = xsTypeName("string")
  val QNAME = xsTypeName("QName")
  val DATE = xsTypeName("date")
  val DATE_TIME = xsTypeName("dateTime")
  val DATE_TIME_STAMP = xsTypeName("dateTimeStamp")

  val NORMALIZED_STRING = xsTypeName("normalizedString")
  val TOKEN = xsTypeName("token")
  val LANGUAGE = xsTypeName("language")
  val NAME = xsTypeName("Name")
  val NC_NAME = xsTypeName("NCName")
  val ENTITY = xsTypeName("ENTITY")
  val ID = xsTypeName("ID")
  val IDREF = xsTypeName("IDREF")
  val NMTOKEN = xsTypeName("NMTOKEN")
}
