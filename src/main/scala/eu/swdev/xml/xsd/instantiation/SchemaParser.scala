package eu.swdev.xml.xsd.instantiation

import java.net.URI

import eu.swdev.xml.log._
import eu.swdev.xml.xsd.cmp.SchemaElem
import eu.swdev.xml.xsd.parser.XsdPushParserMod

trait SchemaParser {

  val xsdParsers: XsdPushParserMod

  def parseSchemaDocument(inputs: xsdParsers.DriveInputs, baseUri: Option[URI]): (Messages, Option[SchemaElem]) = xsdParsers.parseSchemaDocument(inputs, baseUri)

}
