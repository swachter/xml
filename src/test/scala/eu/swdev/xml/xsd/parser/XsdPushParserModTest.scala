package eu.swdev.xml.xsd.parser

import java.io.StringReader
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants, XMLStreamReader}

import eu.swdev.xml.name._
import eu.swdev.xml.pushparser.{XmlEventReaderInputs, XmlPushParserMod}
import eu.swdev.xml.xsd.cmp.{DocumentationElem, AppInfoElem, AnnotationElem}
import org.scalatest.{FunSuite, Inside}

/**
  */
class XsdPushParserModTest extends FunSuite with Inside {

  test("simple") {

    object P extends XmlPushParserMod with XmlEventReaderInputs {

      val parseElement: Parser[(Option[String], String)] = for {
        _ <- startElement(QNameFactory.caching(new Namespace("ns"), new LocalName("e")))
        av <- optionalAttr(QNameFactory.caching(new LocalName("a")))
        bv <- requiredAttr(QNameFactory.caching(new LocalName("b")))
        _ <- endElement
      } yield {
        (av, bv)
      }

      val parseNestedElement: Parser[(Option[String], String, String)] = for {
        _ <- startElement(QNameFactory.caching(new Namespace("ns"), new LocalName("e")))
        av <- optionalAttr(QNameFactory.caching(new LocalName("a")))
        bv <- requiredAttr(QNameFactory.caching(new LocalName("b")))
        _ <- startElement(QNameFactory.caching(new Namespace("ns"), new LocalName("f")))
        cv <- requiredAttr(QNameFactory.caching(new LocalName("c")))
        _ <- endElement
        _ <- endElement
      } yield {
        (av, bv, cv)
      }

      val parseNestedElement2: Parser[(Option[String], String, String)] = for {
        _ <- startElement(QNameFactory.caching(new Namespace("ns"), new LocalName("e")))
        av <- optionalAttr(QNameFactory.caching(new LocalName("a")))
        _ <- startElement(QNameFactory.caching(new Namespace("ns"), new LocalName("f")))
        cv <- requiredAttr(QNameFactory.caching(new LocalName("c")))
        _ <- endElement
        bv <- requiredAttr(QNameFactory.caching(new LocalName("b")))
        _ <- endElement
      } yield {
        (av, bv, cv)
      }

    }

    def eventStream(string: String) = {
      val reader = XMLInputFactory.newInstance().createXMLEventReader(new StringReader(string))
      P.inputs(reader)
    }

    val res = P.document(P.parseElement).drive(P.initialState, eventStream("""<p:e xmlns:p="ns" a="1" b="2"/>"""))

    inside(res) {
      case (Some((Some("1"), "2")), _, _, _) =>
    }

    val res2 = P.document(P.parseNestedElement).drive(P.initialState, eventStream("""<p:e xmlns:p="ns" a="1" b="2"><p:f c="3"/></p:e>"""))

    inside(res2) {
      case (Some((Some("1"), "2", "3")), _, _, _) =>
    }

    val res3 = P.document(P.parseNestedElement2).drive(P.initialState, eventStream("""<p:e xmlns:p="ns" a="1" b="2"><p:f c="3"/></p:e>"""))

    inside(res3) {
      case (Some((Some("1"), "2", "3")), _, _, _) =>
    }


  }

  test("annotation") {

    object P extends XsdPushParserMod with XmlEventReaderInputs

    def eventStream(string: String) = {
      val reader = XMLInputFactory.newInstance().createXMLEventReader(new StringReader(string))
      P.inputs(reader)
    }

    def parseAnnotation(raw: String) = P.document(P.annotation).drive(P.initialState, eventStream(raw))

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
      case (Some(AnnotationElem(_, Some("a1"), AppInfoElem(_, Some("appInfoSource"), _, _) :: Nil, _)), _, _, _) =>
    }

    inside(parseAnnotation(
      """
        |<xs:annotation xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1">
        |  <xs:appinfo source="appInfoSource">some arbitrary content</xs:appinfo>
        |</xs:annotation>
      """.stripMargin)) {
      case (Some(AnnotationElem(_, Some("a1"), AppInfoElem(_, Some("appInfoSource"), "some arbitrary content", _) :: Nil, _)), _, _, _) =>
    }

    inside(parseAnnotation(
      """
        |<xs:annotation xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1">
        |  <xs:appinfo source="appInfoSource">some arbitrary content <b>with</b> markup</xs:appinfo>
        |</xs:annotation>
      """.stripMargin)) {
      case (Some(AnnotationElem(_, Some("a1"), AppInfoElem(_, Some("appInfoSource"), "some arbitrary content <b>with</b> markup", _) :: Nil, _)), _, _, _) =>
    }

    inside(parseAnnotation(
      """
        |<xs:annotation xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1">
        |  <xs:appinfo source="appInfoSource">some arbitrary content <b>with</b> markup</xs:appinfo>
        |  <xs:appinfo source="appInfoSource">some arbitrary content <b>with</b> markup</xs:appinfo>
        |  <xs:appinfo source="appInfoSource">some arbitrary content <b>with</b> markup</xs:appinfo>
        |</xs:annotation>
      """.stripMargin)) {
      case (Some(AnnotationElem(_, Some("a1"), (_: AppInfoElem) :: (_: AppInfoElem) :: (_: AppInfoElem) :: Nil, _)), _, _, _) =>
    }

    inside(parseAnnotation(
      """
        |<xs:annotation xmlns:xs="http://www.w3.org/2001/XMLSchema" id="a1">
        |  <xs:documentation xml:lang="en" source="documentationSource">with some text</xs:documentation>
        |</xs:annotation>
      """.stripMargin)) {
      case (Some(AnnotationElem(_, Some("a1"), DocumentationElem(_, Some("documentationSource"), Some("en"), "with some text", _) :: Nil, _)), _, _, _) =>
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
      case (Some(AnnotationElem(_, Some("a1"), (_: AppInfoElem) :: (_: DocumentationElem) :: (_: AppInfoElem) :: (_: DocumentationElem) :: Nil, _)), _, _, _) =>
    }

  }
}
