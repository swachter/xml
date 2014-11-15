package eu.swdev.xml.xsd.parser

import javax.xml.stream.{XMLStreamConstants, XMLStreamReader}

import eu.swdev.xml.base.Location
import eu.swdev.xml.name.{Namespaces, LocalName, QNameFactory, QName, XsdNamespace, XmlNamespace}
import eu.swdev.xml.pushparser.XmlPushParserMod
import eu.swdev.xml.xsd.cmp.{IncludeElem, DocumentationElem, AppInfoElem, AnnotationElem}
import shapeless.{HList, Generic, HNil, ::}

trait XsdPushParserMod extends XmlPushParserMod {

  type OpenAttrsValue = Map[QName, String]
  type IdAttrValue = Option[String]

  lazy val annotated: Parser[IdAttrValue :: OpenAttrsValue :: Option[AnnotationElem] :: HNil] = { val r = idAttr ~ openAttrs ~ annotation.opt; r }

  lazy val annotation: Parser[AnnotationElem] = xsElem("annotation")(idAttr ~ openAttrs ~ (appInfo | documentation).rep) gmap Generic[AnnotationElem]

  lazy val appInfo: Parser[AppInfoElem] = xsElem("appinfo")(sourceAttr ~ openAttrs ~ rawXml) gmap Generic[AppInfoElem]

  lazy val documentation: Parser[DocumentationElem] = xsElem("documentation")(sourceAttr ~ langAttr ~ openAttrs ~ rawXml) gmap Generic[DocumentationElem]

  lazy val idAttr: Parser[IdAttrValue] = optionalAttr(QNameFactory.caching(new LocalName("id")))

  lazy val include: Parser[IncludeElem] = xsElem("include")(annotated ~ schemaLocationAttr) gmap Generic[IncludeElem]

  lazy val langAttr = optionalAttr(QNameFactory.caching(XmlNamespace, new LocalName("lang")))

  lazy val openAttrs: Parser[OpenAttrsValue] = selectAttrs(_.namespace != XsdNamespace)

  lazy val schemaLocationAttr: Parser[String] = requiredAttr(QNameFactory.caching(new LocalName("schemaLocation")))

  lazy val sourceAttr: Parser[Option[String]] = optionalAttr(QNameFactory.caching(new LocalName("source")))

  private implicit def xsdQName(localName: String): QName = QNameFactory.caching(XsdNamespace, new LocalName(localName))

  private def xsElem[HL <: HList](name: String)(p: Parser[HL]) = startElement(name) ~ p ~ endElement

//  implicit class Ops[HL](p: Parser[HL]) {
//    def gmap[T](generic: Generic[T])(implicit ev: HL <:< generic.Repr): Parser[T] = p map (generic.from(_))
//  }

}
