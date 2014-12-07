package eu.swdev.xml.schema

trait SimpleVal

case class AtomicVal[X](tpe: AtomicType, data: X) extends SimpleVal

case class ListVal(tpe: ListType, data: List[AtomicVal[_]]) extends SimpleVal

