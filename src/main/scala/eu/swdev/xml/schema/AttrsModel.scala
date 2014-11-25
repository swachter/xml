package eu.swdev.xml.schema

import eu.swdev.xml.name.{Namespace, QName}

case class AttrsModel(attrUses: Map[QName, AttrUse], wildcard: Option[Wildcard])

case class AttrUse(decl: AttrDecl, required: Boolean, constraint: Option[ValueConstraint])

case class AttrDecl(name: String, targetNamespace: Namespace, tpe: SimpleType, constraint: Option[ValueConstraint], inheritable: Boolean)

