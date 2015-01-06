package eu.swdev.xml.xsd.testCollection

import java.net.{URI, URL}
import javax.xml.stream.XMLInputFactory
import javax.xml.transform.stream.StreamSource

import eu.swdev.xml.log._
import eu.swdev.xml.name.Namespace
import eu.swdev.xml.pushparser.XmlEventReaderInputs
import eu.swdev.xml.schema.Schema
import eu.swdev.xml.xsd.instantiation.JobMod.SchemaImportHint
import eu.swdev.xml.xsd.instantiation._
import eu.swdev.xml.xsd.parser.XsdPushParserMod
import eu.swdev.xml.xsd.testCollection.XsdTestCollectionParser._
import org.scalatest.{FunSuite, Inside}

/**
  */
class XsdTestCollectionInstanceTest extends FunSuite with Inside {

  object UrlXsdParser extends XsdPushParserMod with XmlEventReaderInputs {
    def inputs(uri: URI): DriveInputs = {
      val reader = XMLInputFactory.newInstance().createXMLEventReader(new StreamSource(uri.toURL.toExternalForm))
      inputs(reader)
    }

    def parseDoc[O](p: Parser[O]): URI => DriveResult[O] = in => document(p).drive(initialState(Some(in), 0), inputs(in))

  }

  val suiteUri = this.getClass.getResource("/xmlschema2006-11-06/suite.xml").toURI

  val suiteParseResult = XsdTestCollectionParser.parseDoc(XsdTestCollectionParser.testSuite)(suiteUri)

  object sut extends SchemaInstantiator with SchemaParser with SimpleSchemaStore with StdResolveImport with SchemaLoader {

    override def builtInSchemas: Iterable[Schema] = Seq(Schema.builtInSchema)

    override val xsdParsers = new XsdPushParserMod with XmlEventReaderInputs {}

    override def resolveInclude(schemaLocation: String, baseUri: Option[URI]): (Messages, Option[(xsdParsers.DriveInputs, Option[URI])]) = {
      (prepend(s"can not resolve schema for schema location: $schemaLocation", emptyMessages), None)
    }

    def parse(uri: URI): (Messages, Option[Schema]) = {
      val source = new StreamSource(uri.toURL.toExternalForm)
      val inputs = xsdParsers.inputs(source)
      loadSchema(inputs, Some(uri))
    }

  }



  for {
    suite <- suiteParseResult._1
    testSetRef <- suite.testSetRefs
    testSetUri = suiteUri.resolve(testSetRef.href)
    testSet <- XsdTestCollectionParser.parseDoc(XsdTestCollectionParser.testSet)(testSetUri)._1
    testGroup <- testSet.testGroups
    schemaTest <- testGroup.schemaTest
    schemaDocument <- schemaTest.schemaDocuments
  } {
    test(s"${testSetRef.href}/${testSet.name}/${testGroup.name}/${schemaDocument.href}") {
      val schemaUri = testSetUri.resolve(schemaDocument.href)
      if (schemaTest.expected.map(_.validity == ValidityOutcome.Invalid).getOrElse(false)) {
        inside(sut.parse(schemaUri)) {
          case (_, None) =>
          case (msgs, _) if (!msgs.isEmpty) =>
        }
      } else {
        inside(sut.parse(schemaUri)) {
          case (msgs, Some(_)) if (msgs.isEmpty) =>
        }
      }

    }
  }

}
