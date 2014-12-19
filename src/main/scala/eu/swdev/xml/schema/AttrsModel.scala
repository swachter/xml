package eu.swdev.xml.schema

import eu.swdev.xml.name.{Namespace, QName}

case class AttrsModel(attrUses: Map[QName, AttrUse], wildcard: Option[Wildcard]) {
  def add(that: AttrsModel): AttrsModel = {
    // 3.6.2.2; not sure
    val wc: Option[Wildcard] = (wildcard, that.wildcard) match {
      case (None, ow) => ow
      case (ow, None) => ow
      case (Some(w1), Some(w2)) => Some(Wildcard(w1.namespaceConstraint.intersect(w2.namespaceConstraint), w1.disallowedNames ++ w2.disallowedNames, w1.processContents))
    }
    AttrsModel(attrUses ++ that.attrUses, wc)
  }
}

object AttrsModel {
  val empty = AttrsModel(Map(), None)
}

case class AttrUse(decl: AttrDecl, use: Use, constraint: Option[ValueConstraint])

case class AttrDecl(name: QName, tpe: SimpleType, constraint: Option[ValueConstraint], inheritable: Boolean)

case class AttrGroup(name: QName, attrsModel: AttrsModel)

