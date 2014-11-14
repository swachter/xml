package eu.swdev.xml.xsd.parser

import javax.xml.stream.{XMLStreamConstants, XMLStreamReader}

import eu.swdev.xml.name.{Namespaces, LocalName, QNameFactory, QName, XsdNamespace, XmlNamespace}
import eu.swdev.xml.pushparser.XmlPushParserMod
import eu.swdev.xml.xsd.cmp.{IncludeElem, DocumentationElem, AppInfoElem, AnnotationElem}
import shapeless.{Generic, HNil, ::}

trait XsdPushParserMod extends XmlPushParserMod {

  type OpenAttrsValue = Map[QName, String]
  type IdAttrValue = Option[String]

  lazy val annotated: Parser[IdAttrValue :: OpenAttrsValue :: Option[AnnotationElem] :: HNil] = { val r = pId ~ pOpenAttrs ~ annotation.opt; r }

  lazy val annotation: Parser[AnnotationElem] =
    startElement("annotation") ~ pId ~ pOpenAttrs ~ (appInfo | documentation).rep ~ endElement map (Generic[AnnotationElem].from(_))

  lazy val appInfo: Parser[AppInfoElem] =
    startElement("appinfo") ~ pSourceAtt ~ pOpenAttrs ~ rawXml ~ endElement map (Generic[AppInfoElem].from(_))

  lazy val documentation: Parser[DocumentationElem] =
    startElement("documentation") ~
      pSourceAtt ~
      optionalAttr(QNameFactory.caching(XmlNamespace, new LocalName("lang"))) ~
      pOpenAttrs ~ rawXml ~ endElement map (Generic[DocumentationElem].from(_))

  lazy val pId: Parser[Option[String]] = optionalAttr(QNameFactory.caching(new LocalName("id")))

  lazy val pInclude: Parser[IncludeElem] =
    startElement("include") ~ annotated ~ pSchemaLocation ~ endElement map (Generic[IncludeElem].from(_))

  lazy val pOpenAttrs: Parser[OpenAttrsValue] = selectAttrs(_.namespace != XsdNamespace)

  lazy val pSchemaLocation: Parser[String] = requiredAttr(QNameFactory.caching(new LocalName("source")))

  lazy val pSourceAtt: Parser[Option[String]] = optionalAttr(QNameFactory.caching(new LocalName("source")))

  private implicit def xsdQName(localName: String): QName = QNameFactory.caching(XsdNamespace, new LocalName(localName))
}
