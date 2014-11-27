package eu.swdev.xml.xsd.testCollection

import java.net.URL
import javax.xml.stream.XMLInputFactory
import javax.xml.transform.stream.StreamSource

import eu.swdev.xml.pushparser.XmlEventReaderInputs
import eu.swdev.xml.xsd.parser.XsdPushParserMod
import eu.swdev.xml.xsd.testCollection.XsdTestCollectionParser._
import org.scalatest.{FunSuite, Inside}

/**
  */
class XsdTestCollectionInstanceTest extends FunSuite with Inside {

  object UrlXsdParser extends XsdPushParserMod with XmlEventReaderInputs {
    def inputs(url: URL): DriveInputs = {
      val reader = XMLInputFactory.newInstance().createXMLEventReader(new StreamSource(url.toExternalForm))
      inputs(reader)
    }

    def parseDoc[O](p: Parser[O]): URL => DriveResult[O] = in => document(p).drive(initialState(0), inputs(in))

  }

  val suiteUrl = this.getClass.getResource("/xmlschema2006-11-06/suite.xml")

  val suiteParseResult = XsdTestCollectionParser.parseDoc(XsdTestCollectionParser.testSuite)(suiteUrl)

  for {
    suite <- suiteParseResult._1
    testSetRef <- suite.testSetRefs
    testSetUrl = new URL(suiteUrl, testSetRef.href)
    testSet <- XsdTestCollectionParser.parseDoc(XsdTestCollectionParser.testSet)(testSetUrl)._1
    testGroup <- testSet.testGroups
    schemaTest <- testGroup.schemaTest
    schemaDocument <- schemaTest.schemaDocuments
  } {
    test(s"${testSetRef.href}/${testSet.name}/${testGroup.name}/${schemaDocument.href}") {
      val schemaUrl = new URL(testSetUrl, schemaDocument.href)
      if (schemaTest.expected.map(_.validity == ValidityOutcome.Invalid).getOrElse(false)) {
        inside(UrlXsdParser.parseDoc(UrlXsdParser.schema)(schemaUrl)) {
          case (None, _, _, _) =>
        }
      } else {
        inside(UrlXsdParser.parseDoc(UrlXsdParser.schema)(schemaUrl)) {
          case (Some(_), _, _, _) =>
        }
      }

    }
  }

}
