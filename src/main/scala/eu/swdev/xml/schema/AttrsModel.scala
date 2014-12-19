package eu.swdev.xml.schema

import eu.swdev.xml.name.{Namespace, QName}

case class AttrsModel(attrUses: Map[QName, AttrUse], wildcard: Option[Wildcard]) {

  /**
   * Adds another attributes model to this model.
   *
   * When the attributes model of a complex type is calculated then the local attributes model should be used
   * as the seed and other attributes models should be added in document order. This is required in order to make
   * the value of the processContents property of the resulting wildcard deterministic.
   *
   * @param that
   * @return
   */
  def add(that: AttrsModel): AttrsModel = {
    // 3.6.2.2
    val wc: Option[Wildcard] = (wildcard, that.wildcard) match {
      case (None, ow) => ow
      case (ow, None) => ow
      case (Some(w1), Some(w2)) => {
        // the processContents property of this wildcard of this AttrsModel is used
        // (the spec requires that the local wildcard is preferred over "included" wildcards and that
        // if no local wildcard is available that then the first wildcard (in document order) is used.
        Some(Wildcard(w1.namespaceConstraint.intersect(w2.namespaceConstraint), w1.processContents))
      }
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

