package eu.swdev.xml.xsd.instantiation

import eu.swdev.xml.log._
import eu.swdev.xml.name
import eu.swdev.xml.name.Namespace
import eu.swdev.xml.schema.Schema

/**
  */
trait SchemaResolver { self: SchemaParser =>

  def resolveSchema(namespace: Namespace, schemaLocation: Option[String]): (Messages, Option[xsdParsers.DriveInputs])

}

trait SchemaLoader { self: SchemaParser with SchemaInstantiator =>

  def loadSchema(inputs: xsdParsers.DriveInputs): (Messages, Option[Schema]) = {
    val (pLog, optSchemaElem) = parseSchemaDocument(inputs)
    optSchemaElem.fold {
      (pLog, Option.empty[Schema])
    } {
      schemaElem => {
        val (iLog, optSchema) = instantiate(schemaElem, schemaElem.targetNamespace.map(Namespace(_)).getOrElse(name.NoNamespace))
        (concat(iLog, pLog), optSchema)
      }
    }
  }

}
