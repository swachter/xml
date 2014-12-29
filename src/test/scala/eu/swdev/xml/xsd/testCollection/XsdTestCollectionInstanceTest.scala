package eu.swdev.xml.xsd.testCollection

import java.net.URL
import javax.xml.stream.XMLInputFactory
import javax.xml.transform.stream.StreamSource

import eu.swdev.xml.log._
import eu.swdev.xml.name.Namespace
import eu.swdev.xml.pushparser.XmlEventReaderInputs
import eu.swdev.xml.schema.Schema
import eu.swdev.xml.xsd.instantiation._
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

  object sut extends SchemaInstantiator with SchemaParser with SimpleSchemaStore with SchemaResolver with SchemaLoader {

    override def builtInSchemas: Iterable[Schema] = Seq(Schema.builtInSchema)

    override val xsdParsers = new XsdPushParserMod with XmlEventReaderInputs {}

    override def resolveSchema(namespace: Namespace, schemaLocation: Option[String]): (Messages, Option[xsdParsers.DriveInputs]) = {
      (prepend("can not resolve schema for namespace", emptyMessages), None)
    }

    def parse(url: URL): (Messages, Option[Schema]) = {
      val source = new StreamSource(url.toExternalForm)
      val inputs = xsdParsers.inputs(source)
      loadSchema(inputs)
    }

  }



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
        inside(sut.parse(schemaUrl)) {
          case (_, None) =>
          case (msgs, _) if (!msgs.isEmpty) =>
        }
      } else {
        inside(sut.parse(schemaUrl)) {
          case (msgs, Some(_)) if (msgs.isEmpty) =>
        }
      }

    }
  }

}
