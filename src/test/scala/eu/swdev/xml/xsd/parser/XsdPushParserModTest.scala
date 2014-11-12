package eu.swdev.xml.xsd.parser

import java.io.StringReader
import javax.xml.stream.{XMLInputFactory, XMLStreamConstants, XMLStreamReader}

import eu.swdev.xml.name._
import eu.swdev.xml.pushparser.{XmlEventReaderInputs, XmlPushParserMod}
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


  }
}
