package eu.swdev.xml.schema

import eu.swdev.xml.name.{Namespace, QName}

case class AttrsModel(attrUses: Map[QName, AttrUse], wildcard: Option[Wildcard]) {
  def add(m: AttrsModel): AttrsModel = {
    val wc: Option[Wildcard] = (wildcard, m.wildcard) match {
      case (None, ow) => ow
      case (ow, None) => ow
      case (Some(w1), Some(w2)) => ??? // w1.add(w2)
    }
    AttrsModel(attrUses ++ m.attrUses, wc)
  }
}

object AttrsModel {
  val empty = AttrsModel(Map(), None)
}

case class AttrUse(decl: AttrDecl, required: Boolean, constraint: Option[ValueConstraint])

case class AttrDecl(name: String, targetNamespace: Namespace, tpe: SimpleType, constraint: Option[ValueConstraint], inheritable: Boolean)

case class AttrGroup(name: String, attrsModel: AttrsModel)

