package eu.swdev.xml.xsd.parser

import javax.xml.stream.{XMLStreamConstants, XMLStreamReader}

import eu.swdev.xml.name.{Namespaces, LocalName, QNameFactory, QName, XsdNamespace, XmlNamespace}
import eu.swdev.xml.pushparser.XmlPushParserMod
import eu.swdev.xml.xsd.cmp.{IncludeElem, DocumentationElem, AppInfoElem, AnnotationElem}
import shapeless.{Generic, HNil, :: => :::}

trait XsdPushParserMod extends XmlPushParserMod {

  type OpenAttrsValue = Map[QName, String]
  type IdAttrValue = Option[String]

//  lazy val annotated: Parser[IdAttrValue ::: OpenAttrsValue ::: Option[AnnotationElem] ::: HNil] = pId :: pOpenAttrs :: annotation.opt :: pNil

  lazy val annotated: Parser[Option[String] ::: Map[QName, String] ::: HNil] = {
    val app = pId :: pOpenAttrs :: pNil
    val ap: Parser.PrependHListParsers[Map[QName, String] ::: HNil, Option[String] ::: HNil] = app
    val p: Parser[IdAttrValue ::: Map[QName, String] ::: HNil] = Parser.PrependHListParsers.toHListParser(ap)
    p
  }

//  lazy val annotated: Parser[Option[String] ::: Map[QName, String] ::: HNil] = {
//    val app = pId.append(pOpenAttrs)
//    val ap: Parser.AppendHListParsers[Option[String] ::: HNil, Map[QName, String] ::: HNil] = app
//    val p: Parser[IdAttrValue ::: Map[QName, String] ::: HNil] = Parser.AppendHListParsers.toHListParser(ap)
//    p
//  }
//
//  val x: Parser[Option[String] ::: HNil] = HListParser.mapHListParser(pId)

//  lazy val annotated: Parser[IdAttrValue ::: OpenAttrsValue ::: Option[AnnotationElem] ::: HNil] = pId.append(pOpenAttrs).append(annotation.opt)

//  lazy val annotation: Parser[AnnotationElem] = for {
//    hl <- startElement("annotation") :: pId :: pOpenAttrs :: (appInfo | documentation).rep :: pNil
//    _ <- endElement
//  } yield {
//    Generic[AnnotationElem].from(hl)
//  }

  lazy val annotation = for {
    loc <- startElement("annotation")
    id <- pId
    oa <- pOpenAttrs
    appInfoOrDocumentations <- (appInfo | documentation).rep
    _ <- endElement
  } yield {
    AnnotationElem(loc, id, oa, appInfoOrDocumentations)
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
  
  lazy val pId = optionalAttr(QNameFactory.caching(new LocalName("id")))

//  lazy val pInclude = for {
//    hl <- new Parser.HListParserOps(pNil)(HListParser.noopHListParser).::(pSchemaLocation)(HListParser.mapHListParser).::(annotated).::(startElement("include"))
////    hl <- startElement("include") :: annotated :: pSchemaLocation :: new Parser.HListParserOps(pNil)(HListParser.noopHListParser)
//    _ <- endElement
//  } yield Generic[IncludeElem].from(hl)

  lazy val pOpenAttrs = selectAttrs(_.namespace != XsdNamespace)

  lazy val pSchemaLocation = requiredAttr(QNameFactory.caching(new LocalName("source")))

  private implicit def xsdQName(localName: String): QName = QNameFactory.caching(XsdNamespace, new LocalName(localName))
}
