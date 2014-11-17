package eu.swdev.xml.xsd.parser

import javax.xml.stream.{XMLStreamConstants, XMLStreamReader}

import eu.swdev.xml.base.Location
import eu.swdev.xml.name.{Namespaces, LocalName, QNameFactory, QName, XsdNamespace, XmlNamespace}
import eu.swdev.xml.pushparser.XmlPushParserMod
import eu.swdev.xml.xsd.cmp._
import shapeless.ops.hlist.Prepend
import shapeless.{HList, Generic, HNil, ::}

trait XsdPushParserMod extends XmlPushParserMod {

  type OpenAttrsValue = Map[QName, String]
  type IdAttrValue = Option[String]

  lazy val annotated: Parser[IdAttrValue :: Option[AnnotationElem] :: HNil] = { val r = idAttr ~ annotation.opt; r }

  lazy val annotation: Parser[AnnotationElem] = xsElem("annotation")(idAttr ~ (appInfo | documentation).rep) gmap Generic[AnnotationElem]

  lazy val appInfo: Parser[AppInfoElem] = xsElem("appinfo")(sourceAttr ~ rawXml) gmap Generic[AppInfoElem]

  lazy val documentation: Parser[DocumentationElem] = xsElem("documentation")(sourceAttr ~ langAttr ~ rawXml) gmap Generic[DocumentationElem]

  def either[L, R](l: Parser[L], r: Parser[R]): Parser[Either[L, R]] = (l map (Left(_))) | (r map (Right(_)))

  lazy val idAttr: Parser[IdAttrValue] = optionalAttr(QNameFactory.caching(new LocalName("id")))

  lazy val include: Parser[IncludeElem] = xsElem("include")(annotated ~ schemaLocationAttr) gmap Generic[IncludeElem]

  lazy val langAttr = optionalAttr(QNameFactory.caching(XmlNamespace, new LocalName("lang")))

  lazy val openAttrs: Parser[OpenAttrsValue] = selectAttrs(_.namespace != XsdNamespace)

  lazy val redefinableGroupElem: Parser[RedefinableGroupElem] = fail("not yet implemented") // simpleType | complexType | group | attributeGroup

  lazy val redefine: Parser[RedefineElem] = xsElem("redefine")(idAttr ~ schemaLocationAttr ~ either(redefinableGroupElem, annotation).rep) gmap Generic[RedefineElem]

  lazy val schemaLocationAttr: Parser[String] = requiredAttr(QNameFactory.caching(new LocalName("schemaLocation")))

  lazy val sourceAttr: Parser[Option[String]] = optionalAttr(QNameFactory.caching(new LocalName("source")))

  private implicit def xsdQName(localName: String): QName = QNameFactory.caching(XsdNamespace, new LocalName(localName))

  lazy val openAttrsEndElem: Parser[OpenAttrsValue :: HNil] = { val r = openAttrs ~ endElement; r }

  private def xsElem[HL <: HList](name: String)(p: Parser[HL])(implicit ev: Prepend[Location :: HL, OpenAttrsValue :: HNil]) = startElement(name) ~ p ~ openAttrsEndElem


}
