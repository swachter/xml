package eu.swdev.xml

package object name {

  import javax.xml.XMLConstants

  val XmlNamespace = Namespace(XMLConstants.XML_NS_URI)
  val XmlPrefix = Prefix(XMLConstants.XML_NS_PREFIX)

  val XmlnsNamespace = Namespace(XMLConstants.XMLNS_ATTRIBUTE_NS_URI)
  val XmlnsPrefix = Prefix(XMLConstants.XMLNS_ATTRIBUTE)

  val NoNamespace = Namespace(XMLConstants.NULL_NS_URI)
  val NoPrefix = Prefix(XMLConstants.DEFAULT_NS_PREFIX)

  val XsdNamespace = Namespace(XMLConstants.W3C_XML_SCHEMA_NS_URI)
}
