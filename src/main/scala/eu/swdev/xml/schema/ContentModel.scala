package eu.swdev.xml.schema

import java.net.URI

/**
  */
sealed trait ContentModel

object EmptyContentModel extends ContentModel

case class SimpleContentModel(tpe: SimpleType) extends ContentModel

case class ComplexContentModel(group: Group, mixed: Boolean, open: Option[OpenContent]) extends ContentModel

sealed trait Group

case class ElemDecl()
