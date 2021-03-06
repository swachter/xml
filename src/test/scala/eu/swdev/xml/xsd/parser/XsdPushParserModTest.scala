package eu.swdev.xml.xsd.parser

import java.io.StringReader
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants, XMLStreamReader}

import eu.swdev.xml.name._
import eu.swdev.xml.pushparser.{XmlEventReaderInputs, XmlPushParserMod}
import eu.swdev.xml.schema.Relation
import eu.swdev.xml.xsd.cmp._
import org.scalatest.{FunSuite, Inside}

/**
  */
class XsdPushParserModTest extends FunSuite with Inside {

  trait StringInputs extends XmlEventReaderInputs { self: XmlPushParserMod =>

    def inputs(string: String): DriveInputs = {
      val reader = XMLInputFactory.newInstance().createXMLEventReader(new StringReader(string))
      inputs(reader)
    }

    def stringParser[O](p: Parser[O]): String => DriveResult[O] = s => parseDocument(p, initialState(None, initialPayload), inputs(s))

    def initialPayload: Payload
  }


  test("simple") {

    object parsers extends XmlPushParserMod with StringInputs {

      val parseElement: Parser[(Option[String], String)] = for {
        _ <- startElement(QNameFactory.caching(Namespace("ns"), LocalName("e")))
        av <- optionalAttr(QNameFactory.caching(LocalName("a")))
        bv <- requiredAttr(QNameFactory.caching(LocalName("b")))
        _ <- endElement
      } yield {
        (av, bv)
      }

      val parseNestedElement: Parser[(Option[String], String, String)] = for {
        _ <- startElement(QNameFactory.caching(Namespace("ns"), LocalName("e")))
        av <- optionalAttr(QNameFactory.caching(LocalName("a")))
        bv <- requiredAttr(QNameFactory.caching(LocalName("b")))
        _ <- startElement(QNameFactory.caching(Namespace("ns"), LocalName("f")))
        cv <- requiredAttr(QNameFactory.caching(LocalName("c")))
        _ <- endElement
        _ <- endElement
      } yield {
        (av, bv, cv)
      }

      val parseNestedElement2: Parser[(Option[String], String, String)] = for {
        _ <- startElement(QNameFactory.caching(Namespace("ns"), LocalName("e")))
        av <- optionalAttr(QNameFactory.caching(LocalName("a")))
        _ <- startElement(QNameFactory.caching(Namespace("ns"), LocalName("f")))
        cv <- requiredAttr(QNameFactory.caching(LocalName("c")))
        _ <- endElement
        bv <- requiredAttr(QNameFactory.caching(LocalName("b")))
        _ <- endElement
      } yield {
        (av, bv, cv)
      }

      type Payload = Unit
      def initialPayload = ()
    }

    val res = parsers.stringParser(parsers.parseElement)("""<p:e xmlns:p="ns" a="1" b="2"/>""")

    inside(res) {
      case (Some((Some("1"), "2")), _, _, _) =>
    }

    val res2 = parsers.stringParser(parsers.parseNestedElement)("""<p:e xmlns:p="ns" a="1" b="2"><p:f c="3"/></p:e>""")

    inside(res2) {
      case (Some((Some("1"), "2", "3")), _, _, _) =>
    }

    val res3 = parsers.stringParser(parsers.parseNestedElement2)("""<p:e xmlns:p="ns" a="1" b="2"><p:f c="3"/></p:e>""")

    inside(res3) {
      case (Some((Some("1"), "2", "3")), _, _, _) =>
    }


  }

  object xsdParsers extends XsdPushParserMod with StringInputs {
    val initialPayload = 0
  }

  test("annotation") {

    def parseAnnotation(raw: String) = xsdParsers.stringParser(xsdParsers.annotation)(raw)

    inside(parseAnnotation(
      """
        |<xs:annotation xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1"/>
      """.stripMargin)) {
      case (Some(AnnotationElem(_, Some("a1"), _, _)), _, _, _) =>
    }

    inside(parseAnnotation(
      """
        |<xs:annotation xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1">
        |  <xs:appinfo source="appInfoSource"/>
        |</xs:annotation>
      """.stripMargin)) {
      case (Some(AnnotationElem(_, Some("a1"), Left(AppInfoElem(_, Some("appInfoSource"), _, _)) :: Nil, _)), _, _, _) =>
    }

    inside(parseAnnotation(
      """
        |<xs:annotation xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1">
        |  <xs:appinfo source="appInfoSource">some arbitrary content</xs:appinfo>
        |</xs:annotation>
      """.stripMargin)) {
      case (Some(AnnotationElem(_, Some("a1"), Left(AppInfoElem(_, Some("appInfoSource"), "some arbitrary content", _)) :: Nil, _)), _, _, _) =>
    }

    inside(parseAnnotation(
      """
        |<xs:annotation xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1">
        |  <xs:appinfo source="appInfoSource">some arbitrary content <b>with</b> markup</xs:appinfo>
        |</xs:annotation>
      """.stripMargin)) {
      case (Some(AnnotationElem(_, Some("a1"), Left(AppInfoElem(_, Some("appInfoSource"), "some arbitrary content <b>with</b> markup", _)) :: Nil, _)), _, _, _) =>
    }

    inside(parseAnnotation(
      """
        |<xs:annotation xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1">
        |  <xs:appinfo source="appInfoSource">some arbitrary content <b>with</b> markup</xs:appinfo>
        |  <xs:appinfo source="appInfoSource">some arbitrary content <b>with</b> markup</xs:appinfo>
        |  <xs:appinfo source="appInfoSource">some arbitrary content <b>with</b> markup</xs:appinfo>
        |</xs:annotation>
      """.stripMargin)) {
      case (Some(AnnotationElem(_, Some("a1"), (_: Left[AppInfoElem, DocumentationElem]) :: (_: Left[AppInfoElem, DocumentationElem]) :: (_: Left[AppInfoElem, DocumentationElem]) :: Nil, _)), _, _, _) =>
    }

    inside(parseAnnotation(
      """
        |<xs:annotation xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1">
        |  <xs:documentation xml:lang="en" source="documentationSource">with some text</xs:documentation>
        |</xs:annotation>
      """.stripMargin)) {
      case (Some(AnnotationElem(_, Some("a1"), Right(DocumentationElem(_, Some("documentationSource"), Some("en"), "with some text", _)) :: Nil, _)), _, _, _) =>
    }

    inside(parseAnnotation(
      """
        |<xs:annotation xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1">
        |  <xs:appinfo source="appInfoSource">some arbitrary content <b>with</b> markup</xs:appinfo>
        |  <xs:documentation xml:lang="en" source="documentationSource">with some text</xs:documentation>
        |  <xs:appinfo source="appInfoSource">some arbitrary content <b>with</b> markup</xs:appinfo>
        |  <xs:documentation xml:lang="en" source="documentationSource">with some text</xs:documentation>
        |</xs:annotation>
      """.stripMargin)) {
      case (Some(AnnotationElem(_, Some("a1"), (_: Left[AppInfoElem, DocumentationElem]) :: (_: Right[AppInfoElem, DocumentationElem]) :: (_: Left[AppInfoElem, DocumentationElem]) :: (_: Right[AppInfoElem, DocumentationElem]) :: Nil, _)), _, _, _) =>
    }

  }

  test("union") {

    def parse(raw: String) = xsdParsers.stringParser(xsdParsers.union)(raw)

    inside(parse(
      """
        |<xs:union xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1" memberTypes="a xs:b"/>
      """.stripMargin)) {
      case (Some(UnionElem(_, _, _, Some(QName(Namespace(""), LocalName("a"), _) :: QName(XsdNamespace, LocalName("b"), _) :: Nil), _, _)), _, _, _) =>
    }

  }

  test("derivation control") {

    import xsdParsers._

    def parse[C <: Relation](string: String)(pf: PartialFunction[String, C]) =
      derivationCtrlStr(success(string))(pf).drive(initialState(None, 0), Stream.empty)

    inside(parse("#all")(relExtension orElse relRestriction)) {
      case (Some(RelationSet.All), _, _, _) =>
    }

    inside(parse("extension restriction")(relExtension orElse relRestriction)) {
      case (Some(RelationSet.Items(Relation.Extension :: Relation.Restriction :: Nil)), _, _, _) =>
    }

    inside(parse("restriction extension")(relExtension orElse relRestriction)) {
      case (Some(RelationSet.Items(Relation.Restriction :: Relation.Extension :: Nil)), _, _, _) =>
    }


  }

}
