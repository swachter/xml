package eu.swdev.xml.schema

import java.net.URI

/**
  */
sealed trait ContentModel {
  def simpleTypeDefinition: Option[SimpleType]
  def isMixedAndEmptiable: Boolean
}

object EmptyContentModel extends ContentModel {
  override def simpleTypeDefinition = None
  override def isMixedAndEmptiable = false
}

case class SimpleContentModel(tpe: SimpleType) extends ContentModel {
  override def simpleTypeDefinition = Some(tpe)
  override def isMixedAndEmptiable = false
}

case class ComplexContentModel(group: Group, mixed: Boolean, open: Option[OpenContent]) extends ContentModel {
  override def simpleTypeDefinition = None
  override def isMixedAndEmptiable = mixed && group.isEmptiable
}

sealed trait Group {
  def isEmptiable: Boolean
}

case class ElemDecl()
