package eu.swdev.xml

package object name {

  import javax.xml.XMLConstants

  val XmlNamespace = new Namespace(XMLConstants.XML_NS_URI)
  val XmlPrefix = new Prefix(XMLConstants.XML_NS_PREFIX)

  val XmlnsNamespace = new Namespace(XMLConstants.XMLNS_ATTRIBUTE_NS_URI)
  val XmlnsPrefix = new Prefix(XMLConstants.XMLNS_ATTRIBUTE)

  val NoNamespace = new Namespace(XMLConstants.NULL_NS_URI)
  val NoPrefix = new Prefix(XMLConstants.DEFAULT_NS_PREFIX)

  val XsdNamespace = new Namespace(XMLConstants.W3C_XML_SCHEMA_NS_URI)
}
