package eu.swdev.xml.xsd.instantiation

import java.util

import eu.swdev.xml.log._
import eu.swdev.xml.name.Namespace
import eu.swdev.xml.schema.Schema

trait SchemaStore {

  def getSchema(namespace: Namespace, schemaLocation: Option[String]): (Messages, Option[Schema])

}

trait SimpleSchemaStore extends SchemaStore { self: SchemaParser with SchemaInstantiator =>

  def builtInSchemas: Iterable[Schema]

  private val loadedSchemas = new util.HashMap[Namespace, (Messages, Option[Schema])]()

  private lazy val builtIn = builtInSchemas.map(s => s.namespace -> (emptyMessages, Some(s))).toMap

  def schemaImport(namespace: Namespace, schemaLocation: Option[String]): (Messages, Option[Schema]) = {
    builtIn.getOrElse(namespace, {
      loadedSchemas.synchronized {
        val t = loadedSchemas.get(namespace)
        if (t == null) {
          val (log, optSchemaElem) = parseSchema(namespace, schemaLocation)
          val tt = optSchemaElem.fold {
            (log, Option.empty[Schema])
          } {
            schemaElem => {
              val (l, optSchema) = instantiate(schemaElem, namespace)
              (concat(l, log), optSchema)
            }
          }
          loadedSchemas.put(namespace, tt)
          tt
        } else {
          t
        }
      }
    })
  }

}
