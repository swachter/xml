package eu.swdev.xml.schema

import java.net.URI

import eu.swdev.xml.name.{Namespace, QName}

sealed trait Relation
sealed trait CtDerivationMethod extends Relation
sealed trait StDerivationMethod extends Relation
sealed trait CtBlockCtrl extends Relation
sealed trait CtFinalCtrl extends Relation
sealed trait ElemBlockCtrl extends Relation
sealed trait ElemFinalCtrl extends Relation

object Relation {
  object Extension    extends CtDerivationMethod                         with CtBlockCtrl with CtFinalCtrl with ElemBlockCtrl with ElemFinalCtrl
  object Restriction  extends CtDerivationMethod with StDerivationMethod with CtBlockCtrl with CtFinalCtrl with ElemBlockCtrl with ElemFinalCtrl
  object Substitution extends                                                                                   ElemBlockCtrl
  object List         extends                         StDerivationMethod
  object Union        extends                         StDerivationMethod
}

case class DisallowedNames(qNames: Set[QName], defined: Boolean, sibling: Boolean)

object DisallowedNames {
  val empty = DisallowedNames(Set[QName](), false, false)
}

trait ExplicitTimezone

object ExplicitTimezone {
  object Optional extends ExplicitTimezone
  object Required extends ExplicitTimezone
  object Prohibited extends ExplicitTimezone
}

sealed trait Form

object Form {
  object Qualified extends Form
  object Unqualified extends Form
}

sealed trait MaxOccurs

object MaxOccurs {
  case object Unbounded extends MaxOccurs
  case class Bounded(max: Int) extends MaxOccurs
}

sealed trait NamespaceConstraint 

object NamespaceConstraint {

  case object Any extends NamespaceConstraint
  case class Enum(namespaces: Set[Namespace]) extends NamespaceConstraint
  case class Not(namespaces: Set[Namespace]) extends NamespaceConstraint

}

sealed trait OpenContentMode

object OpenContentMode {
  object Interleave extends OpenContentMode
  object Suffix extends OpenContentMode
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
