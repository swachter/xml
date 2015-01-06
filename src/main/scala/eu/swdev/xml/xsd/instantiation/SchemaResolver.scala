package eu.swdev.xml.xsd.instantiation

import java.net.URI

import eu.swdev.xml.log._
import eu.swdev.xml.name
import eu.swdev.xml.name.Namespace
import eu.swdev.xml.schema.Schema
import eu.swdev.xml.xsd.instantiation.JobMod.SchemaImportHint

/**
  */
trait SchemaResolver { self: SchemaParser =>

  def resolveImport(namespace: Namespace, importHint: SchemaImportHint): (Messages, Option[(xsdParsers.DriveInputs, Option[URI])])

  def resolveInclude(schemaLocation: String, baseUri: Option[URI]): (Messages, Option[(xsdParsers.DriveInputs, Option[URI])])

}

trait StdResolveImport extends SchemaResolver { self: SchemaParser =>

  override def resolveImport(namespace: Namespace, importHint: SchemaImportHint): (Messages, Option[(xsdParsers.DriveInputs, Option[URI])]) = {
    importHint.schemaLocation.fold[(Messages, Option[(xsdParsers.DriveInputs, Option[URI])])] {
      (prepend(s"can not import namespace: $namespace - missing schema location attribute", emptyMessages), None)
    } {
      sl => resolveInclude(sl, importHint.baseUri)
    }
  }

}

trait SchemaLoader { self: SchemaParser with SchemaInstantiator =>

  def loadSchema(inputs: xsdParsers.DriveInputs, baseUri: Option[URI]): (Messages, Option[Schema]) = {
    val (pLog, optSchemaElem) = parseSchemaDocument(inputs, baseUri)
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
