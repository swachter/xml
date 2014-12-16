package eu.swdev.xml.xsd.instantiation

import eu.swdev.xml.log._
import eu.swdev.xml.name.Namespace
import eu.swdev.xml.xsd.cmp.SchemaElem

trait SchemaParser {

  def parseSchema(namespace: Namespace, schemaLocation: Option[String]): (Messages, Option[SchemaElem])

}

