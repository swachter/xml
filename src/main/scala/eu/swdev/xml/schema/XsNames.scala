package eu.swdev.xml.schema

import javax.xml.XMLConstants

import eu.swdev.xml.name.{LocalName, Namespace, Prefix, QNameFactory}

/**
  */
object XsNames {

  val XS_NAMESPACE = new Namespace(XMLConstants.W3C_XML_SCHEMA_NS_URI)
  val XS_PREFIX = new Prefix("xs")

  private def xsTypeName(localName: String) = QNameFactory.caching(XS_NAMESPACE, new LocalName(localName), XS_PREFIX)

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

  val STRING = xsTypeName("string")
  val QNAME = xsTypeName("QName")

}
