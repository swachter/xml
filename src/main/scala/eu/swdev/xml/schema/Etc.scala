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

case class DisallowedNames(names: Set[QName], defined: Boolean, sibling: Boolean) {
  def ++(that: DisallowedNames): DisallowedNames = {
    DisallowedNames(names ++ that.names, defined || that.defined, sibling || that.sibling)
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

  def disallowedNames: DisallowedNames

  // 3.10.4.2
  def isAllowed(name: QName): Boolean = {
    isAllowed(name.namespace) && !disallowedNames.names.contains(name)
  }

  // 3.10.4.3
  def isAllowed(namespace: Namespace): Boolean

  // 3.10.6.3
  def union(that: NamespaceConstraint): NamespaceConstraint = {
    val newDisallowedNames = DisallowedNames(
      disallowedNames.names.filter(!that.isAllowed(_)) ++ that.disallowedNames.names.filter(!this.isAllowed(_)),
      disallowedNames.defined && that.disallowedNames.defined,
      disallowedNames.sibling && that.disallowedNames.sibling
    )
    (this, that) match {
      case (NamespaceConstraint.Any(_), _) => NamespaceConstraint.Any(newDisallowedNames)
      case (_, NamespaceConstraint.Any(_)) => NamespaceConstraint.Any(newDisallowedNames)
      case (NamespaceConstraint.Enum(_, set1), NamespaceConstraint.Enum(_, set2)) => NamespaceConstraint.Enum(newDisallowedNames, set1 ++ set2)
      case (NamespaceConstraint.Enum(_, set1), NamespaceConstraint.Not(_, set2)) => anyOrNot(newDisallowedNames, set2 -- set1)
      case (NamespaceConstraint.Not(_, set1), NamespaceConstraint.Enum(_, set2)) => anyOrNot(newDisallowedNames, set1 -- set2)
      case (NamespaceConstraint.Not(_, set1), NamespaceConstraint.Not(_, set2)) => anyOrNot(newDisallowedNames, set1 intersect set2)
    }
  }

  // 3.10.6.4
  def intersect(that: NamespaceConstraint): NamespaceConstraint = {
    val newDisallowedNames = DisallowedNames(
      disallowedNames.names.filter(that.isAllowed(_)) ++ that.disallowedNames.names.filter(this.isAllowed(_)),
      disallowedNames.defined || that.disallowedNames.defined,
      disallowedNames.sibling || that.disallowedNames.sibling
    )
    (this, that) match {
      case (NamespaceConstraint.Any(_), _) => that.withDisallowedNames(newDisallowedNames)
      case (_, NamespaceConstraint.Any(_)) => this.withDisallowedNames(newDisallowedNames)
      case (NamespaceConstraint.Enum(_, set1), NamespaceConstraint.Enum(_, set2)) => NamespaceConstraint.Enum(newDisallowedNames, set1 intersect set2)
      case (NamespaceConstraint.Enum(_, set1), NamespaceConstraint.Not(_, set2)) => NamespaceConstraint.Enum(newDisallowedNames, set1 -- set2)
      case (NamespaceConstraint.Not(_, set1), NamespaceConstraint.Enum(_, set2)) => NamespaceConstraint.Enum(newDisallowedNames, set2 -- set1)
      case (NamespaceConstraint.Not(_, set1), NamespaceConstraint.Not(_, set2)) => anyOrNot(newDisallowedNames, set1 ++ set2)
    }
  }

  def withDisallowedNames(disallowedNames: DisallowedNames): NamespaceConstraint

  private def anyOrNot(disallowedNames: DisallowedNames, set: Set[Namespace]): NamespaceConstraint =
    if (set.isEmpty) NamespaceConstraint.Any(disallowedNames) else NamespaceConstraint.Not(disallowedNames, set)
}

object NamespaceConstraint {

  case class Any(disallowedNames: DisallowedNames) extends NamespaceConstraint {
    override def isAllowed(namespace: Namespace): Boolean = true
    override def withDisallowedNames(dns: DisallowedNames) = Any(dns)

  }
  case class Enum(disallowedNames: DisallowedNames, namespaces: Set[Namespace]) extends NamespaceConstraint {
    override def isAllowed(namespace: Namespace): Boolean = namespaces.contains(namespace)
    override def withDisallowedNames(dns: DisallowedNames) = Enum(dns, namespaces)
  }
  case class Not(disallowedNames: DisallowedNames, namespaces: Set[Namespace]) extends NamespaceConstraint {
    override def isAllowed(namespace: Namespace): Boolean = !namespaces.contains(namespace)
    override def withDisallowedNames(dns: DisallowedNames) = Not(dns, namespaces)
  }

}

sealed trait OpenContentMode

sealed trait DefaultOpenContentMode extends OpenContentMode

object OpenContentMode {
  object None extends OpenContentMode
  object Interleave extends DefaultOpenContentMode
  object Suffix extends DefaultOpenContentMode
}

case class OpenContent(mode: OpenContentMode, wildcard: Wildcard)

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

case class Wildcard(namespaceConstraint: NamespaceConstraint, processContents: ProcessContents)

sealed trait XPathDefaultNamespace

object XPathDefaultNamespace {

  case object Default extends XPathDefaultNamespace
  case object Target extends XPathDefaultNamespace
  case object Local extends XPathDefaultNamespace
  sealed case class Uri(uri: URI) extends XPathDefaultNamespace

}
