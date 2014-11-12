package eu.swdev.xml.xsd.parser

import javax.xml.stream.{XMLStreamConstants, XMLStreamReader}

import eu.swdev.xml.name.{Namespaces, LocalName, QNameFactory, QName, XsdNamespace, XmlNamespace}
import eu.swdev.xml.pushparser.XmlPushParserMod
import eu.swdev.xml.xsd.cmp.{DocumentationElem, AppInfoElem, AnnotationElem}

trait XsdPushParserMod extends XmlPushParserMod {

  type OpenAttrsValue = Map[QName, String]
  type IdAttrValue = Option[String]

  val openAttrs: Parser[OpenAttrsValue] = selectAttrs(_.namespace != XsdNamespace)

  val annotated: Parser[(OpenAttrsValue, IdAttrValue, Option[AnnotationElem])] = for {
    oa <- openAttrs
    id <- optionalAttr(QNameFactory.caching(new LocalName("id")))
    ae <- annotation.opt
  } yield {
    (oa, id, ae)
  }

  val annotation = for {
    loc <- startElement("annotation")
    id <- optionalAttr(QNameFactory.caching(new LocalName("id")))
    oa <- openAttrs
    appInfoOrDocumentations <- (appInfo | documentation).rep
    _ <- endElement
  } yield {
    AnnotationElem(loc, oa, id, appInfoOrDocumentations)
  }

  val appInfo = for {
    loc <- startElement("appinfo")
    oa <- openAttrs
    src <- optionalAttr(QNameFactory.caching(new LocalName("source")))
    raw <- rawXml
    _ <- endElement
  } yield {
    AppInfoElem(loc, oa, src, raw)
  }

  val documentation = for {
    loc <- startElement("documentation")
    src <- optionalAttr(QNameFactory.caching(new LocalName("source")))
    lang <- optionalAttr(QNameFactory.caching(XmlNamespace, new LocalName("lang")))
    oa <- openAttrs
    raw <- rawXml
    _ <- endElement
  } yield {
    DocumentationElem(loc, oa, src, raw, lang)
  }

  private implicit def xsdQName(localName: String): QName = QNameFactory.caching(XsdNamespace, new LocalName(localName))
}
