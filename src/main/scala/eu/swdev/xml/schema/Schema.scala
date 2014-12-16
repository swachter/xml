package eu.swdev.xml.schema

import eu.swdev.xml.name.{LocalName, Namespace}

trait SchemaTopComponent

class SymbolTable private (underlying: Map[(LocalName, SymbolSpace[_]), Any]) {

  def +[X](kv: (LocalName, X))(implicit ev: SymbolSpace[X]): SymbolTable = new SymbolTable(underlying + ((kv._1, ev) -> kv._2))
  def get[X](name: LocalName)(implicit ev: SymbolSpace[X]): Option[ev.SymbolType] = underlying.get((name, ev)).asInstanceOf[Option[ev.SymbolType]]

}

object SymbolTable {
  val empty = new SymbolTable(Map.empty)
}

/**
  */
case class Schema(namespace: Namespace, symbolTable: SymbolTable) {


}

