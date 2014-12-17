package eu.swdev.xml.xsd.instantiation

import javax.xml.stream.XMLInputFactory
import javax.xml.transform.Source

import eu.swdev.xml.log._
import eu.swdev.xml.name.Namespace
import eu.swdev.xml.pushparser.XmlEventReaderInputs
import eu.swdev.xml.xsd.cmp.SchemaElem
import eu.swdev.xml.xsd.parser.XsdPushParserMod
import org.xml.sax.InputSource

trait SchemaParser {

  val xsdParsers: XsdPushParserMod

  def parseSchemaDocument(inputs: xsdParsers.DriveInputs): (Messages, Option[SchemaElem]) = xsdParsers.parseSchemaDocument(inputs)

}
