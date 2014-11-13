package eu.swdev.xml.xsd.parser

import javax.xml.stream.{XMLStreamConstants, XMLStreamReader}

import eu.swdev.xml.name.{Namespaces, LocalName, QNameFactory, QName, XsdNamespace, XmlNamespace}
import eu.swdev.xml.pushparser.XmlPushParserMod
import eu.swdev.xml.xsd.cmp.{DocumentationElem, AppInfoElem, AnnotationElem}

trait XsdPushParserMod extends XmlPushParserMod {

  type OpenAttrsValue = Map[QName, String]
  type IdAttrValue = Option[String]

  lazy val annotated: Parser[(OpenAttrsValue, IdAttrValue, Option[AnnotationElem])] = for {
    (id, oa) <- pId & pOpenAttrs
    ae <- annotation.opt
  } yield {
    (oa, id, ae)
  }

  lazy val annotation = for {
    loc <- startElement("annotation")
    (id, oa) <- pId & pOpenAttrs
    appInfoOrDocumentations <- (appInfo | documentation).rep
    _ <- endElement
  } yield {
    AnnotationElem(loc, oa, id, appInfoOrDocumentations)
  }

  lazy val appInfo = for {
    loc <- startElement("appinfo")
    oa <- pOpenAttrs
    src <- optionalAttr(QNameFactory.caching(new LocalName("source")))
    raw <- rawXml
    _ <- endElement
  } yield {
    AppInfoElem(loc, oa, src, raw)
  }

  lazy val documentation = for {
    loc <- startElement("documentation")
    src <- optionalAttr(QNameFactory.caching(new LocalName("source")))
    lang <- optionalAttr(QNameFactory.caching(XmlNamespace, new LocalName("lang")))
    oa <- pOpenAttrs
    raw <- rawXml
    _ <- endElement
  } yield {
    DocumentationElem(loc, oa, src, raw, lang)
  }
  
  lazy val pId = for {
    id <- optionalAttr(QNameFactory.caching(new LocalName("id")))
  } yield {
    id
  }

  lazy val pOpenAttrs: Parser[OpenAttrsValue] = selectAttrs(_.namespace != XsdNamespace)

  private implicit def xsdQName(localName: String): QName = QNameFactory.caching(XsdNamespace, new LocalName(localName))
}
