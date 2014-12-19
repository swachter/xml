package eu.swdev.xml.schema

import java.net.URI

import eu.swdev.xml.name.{Namespace, QName}

sealed trait Relation
sealed trait TypeDerivationCtrl extends Relation
sealed trait CtDerivationCtrl extends TypeDerivationCtrl
sealed trait StDerivationCtrl extends TypeDerivationCtrl
sealed trait BlockCtrl extends Relation
sealed trait CtBlockCtrl extends BlockCtrl
sealed trait ElemBlockCtrl extends BlockCtrl
sealed trait ElemFinalCtrl extends Relation

object Relation {
  object Extension    extends CtDerivationCtrl                       with CtBlockCtrl with ElemBlockCtrl with ElemFinalCtrl
  object Restriction  extends CtDerivationCtrl with StDerivationCtrl with CtBlockCtrl with ElemBlockCtrl with ElemFinalCtrl
  object Substitution extends                                                                               ElemBlockCtrl
  object List         extends                       StDerivationCtrl
  object Union        extends                       StDerivationCtrl

  val all = Seq(Extension, Restriction, Substitution, List, Union)
}

case class DisallowedNames(qNames: Set[QName], defined: Boolean, sibling: Boolean) {
  def ++(that: DisallowedNames): DisallowedNames = {
    DisallowedNames(qNames ++ that.qNames, defined || that.defined, sibling || that.sibling)
  }
}

object DisallowedNames {
  val empty = DisallowedNames(Set[QName](), false, false)
}

trait ExplicitTimeZone

object ExplicitTimeZone {
  object Optional extends ExplicitTimeZone
  object Required extends ExplicitTimeZone
  object Prohibited extends ExplicitTimeZone
}

sealed trait Form

object Form {
  object Qualified extends Form
  object Unqualified extends Form
}

case class Occurs(min: Int, max: MaxOccurs)

sealed trait MaxOccurs {
  def isUnbounded: Boolean

  def +(that: MaxOccurs): MaxOccurs = (this, that) match {
    case (MaxOccurs.Bounded(m1), MaxOccurs.Bounded(m2)) => MaxOccurs.Bounded(m1 + m2)
    case _ => MaxOccurs.Unbounded
  }

  def *(that: MaxOccurs): MaxOccurs = (this, that) match {
    case (MaxOccurs.Bounded(m1), MaxOccurs.Bounded(m2)) => MaxOccurs.Bounded(m1 * m2)
    case (MaxOccurs.Bounded(0), _) => MaxOccurs.Bounded(0)
    case (_, MaxOccurs.Bounded(0)) => MaxOccurs.Bounded(0)
    case _ => MaxOccurs.Unbounded
  }

  def max(that: MaxOccurs): MaxOccurs = (this, that) match {
    case (MaxOccurs.Bounded(m1), MaxOccurs.Bounded(m2)) => MaxOccurs.Bounded(m1 max m2)
    case _ => MaxOccurs.Unbounded
  }

}

object MaxOccurs {
  val zero: MaxOccurs = MaxOccurs.Bounded(0)
  val one: MaxOccurs = MaxOccurs.Bounded(1)
  case object Unbounded extends MaxOccurs {
    def isUnbounded = true
  }
  case class Bounded(max: Int) extends MaxOccurs {
    def isUnbounded = false
  }
}

sealed trait NamespaceConstraint {
  // 3.10.6.3
  def union(that: NamespaceConstraint): NamespaceConstraint = {
    (this, that) match {
      case (NamespaceConstraint.Any, _) => NamespaceConstraint.Any
      case (_, NamespaceConstraint.Any) => NamespaceConstraint.Any
      case (NamespaceConstraint.Enum(set1), NamespaceConstraint.Enum(set2)) => NamespaceConstraint.Enum(set1 ++ set2)
      case (NamespaceConstraint.Enum(set1), NamespaceConstraint.Not(set2)) => anyOrNot(set2 -- set1)
      case (NamespaceConstraint.Not(set1), NamespaceConstraint.Enum(set2)) => anyOrNot(set1 -- set2)
      case (NamespaceConstraint.Not(set1), NamespaceConstraint.Not(set2)) => anyOrNot(set1 intersect set2)
    }
  }

  // 3.10.6.4
  def intersect(that: NamespaceConstraint): NamespaceConstraint = {
    (this, that) match {
      case (NamespaceConstraint.Any, _) => that
      case (_, NamespaceConstraint.Any) => this
      case (NamespaceConstraint.Enum(set1), NamespaceConstraint.Enum(set2)) => NamespaceConstraint.Enum(set1 intersect set2)
      case (NamespaceConstraint.Enum(set1), NamespaceConstraint.Not(set2)) => NamespaceConstraint.Enum(set1 -- set2)
      case (NamespaceConstraint.Not(set1), NamespaceConstraint.Enum(set2)) => NamespaceConstraint.Enum(set2 -- set1)
      case (NamespaceConstraint.Not(set1), NamespaceConstraint.Not(set2)) => anyOrNot(set1 ++ set2)
    }
  }

  private def anyOrNot(set: Set[Namespace]): NamespaceConstraint = if (set.isEmpty) NamespaceConstraint.Any else NamespaceConstraint.Not(set)
}

object NamespaceConstraint {

  case object Any extends NamespaceConstraint
  case class Enum(namespaces: Set[Namespace]) extends NamespaceConstraint
  case class Not(namespaces: Set[Namespace]) extends NamespaceConstraint

}

sealed trait OpenContentMode

sealed trait DefaultOpenContentMode extends OpenContentMode

object OpenContentMode {
  object None extends OpenContentMode
  object Interleave extends DefaultOpenContentMode
  object Suffix extends DefaultOpenContentMode
}

case class OpenContent(mode: OpenContentMode, any: OpenContentAny)

case class OpenContentAny(namespaceConstraint: NamespaceConstraint, processContents: ProcessContents)

sealed trait ProcessContents

object ProcessContents {
  object Strict extends ProcessContents
  object Lax extends ProcessContents
  object Skip extends ProcessContents
}

sealed trait QNameItem // xs:qnameList

sealed trait QNameItemA extends QNameItem // xs:qnameListA

object QNameItem {

  sealed case class Qn(qn: QName) extends QNameItemA

  object Defined extends QNameItemA
  object DefinedSibling extends QNameItem
}

sealed trait Use

object Use {
  object Optional extends Use
  object Required extends Use
  object Prohibited extends Use
}

case class ValueConstraint(lexicalForm: String, defaultNotFixed: Boolean)

case class Wildcard(namespaceConstraint: NamespaceConstraint, disallowedNames: DisallowedNames, processContents: ProcessContents)

sealed trait XPathDefaultNamespace

object XPathDefaultNamespace {

  case object Default extends XPathDefaultNamespace
  case object Target extends XPathDefaultNamespace
  case object Local extends XPathDefaultNamespace
  sealed case class Uri(uri: URI) extends XPathDefaultNamespace

}
