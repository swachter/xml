package eu.swdev.xml.xsd.instantiation

import java.util

import eu.swdev.xml.log._
import eu.swdev.xml.name.Namespace
import eu.swdev.xml.schema.Schema
import eu.swdev.xml.xsd.cmp.SchemaElem

trait SchemaStore {

  def importSchema(namespace: Namespace, importHint: SchemaImportHint): (Messages, Option[Schema])

}

trait SimpleSchemaStore extends SchemaStore { self: SchemaParser with SchemaInstantiator with SchemaResolver with SchemaLoader =>

  def builtInSchemas: Iterable[Schema]

  private val loadedSchemas = new util.HashMap[Namespace, (Messages, Option[Schema])]()

  private lazy val builtIn = builtInSchemas.map(s => s.namespace -> (emptyMessages, Some(s))).toMap

  override def importSchema(namespace: Namespace, importHint: SchemaImportHint): (Messages, Option[Schema]) = {
    builtIn.getOrElse(namespace, {
      loadedSchemas.synchronized {
        val t = loadedSchemas.get(namespace)
        if (t == null) {
          
          val (rLog, optResolved) = resolveImport(namespace, importHint)
          
          val res@(log2, optSchema) = optResolved.fold {
            (rLog, Option.empty[Schema])
          } {
            resolved => {
              val (pLog, optSchema) = loadSchema(resolved._1, resolved._2)
              (concat(pLog, rLog), optSchema)
            }
          }

          loadedSchemas.put(namespace, res)
          res
        } else {
          t
        }
      }
    })
  }

}
