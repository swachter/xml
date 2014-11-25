package eu.swdev.xml.xsd.cmp

import java.net.URI

import eu.swdev.xml.base.Location
import eu.swdev.xml.name.QName

/**
 *
 */

sealed trait Annotated extends OpenAttrs {
  def annotation: Option[AnnotationElem]
  def id: Option[String]
}

sealed trait ComplexType extends Annotated

sealed trait Located {
  def loc: Location
}

sealed trait OpenAttrs extends Located {
  def openAttrs: Map[QName, String]
}

//
// groups & substitution groups
//

sealed trait CompositionGroupElem

sealed trait FacetElem

sealed trait IdentityConstraintGroupElem

sealed trait RedefinableGroupElem extends SchemaTopGroupElem

sealed trait SchemaTopGroupElem

sealed trait SimpleDerivationGroupElem

//

sealed trait Particle

sealed trait NestedParticle extends Particle

sealed trait InGroupDefParticle extends Particle

sealed trait TypeDefParticle extends Particle

//

sealed trait ComplexDerivation

sealed trait SimpleDerivation

sealed trait ComplexTypeContent

sealed trait ComplexContent extends ComplexTypeContent

//
//
//

case class AllElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], particles: Seq[NestedParticle], openAttrs: Map[QName, String]) extends InGroupDefParticle with TypeDefParticle

case class AlternativeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], test: Option[String], refType: Option[QName], xPathDefaultNamespace: Option[XPathDefaultNamespaceToken], inlinedType: Option[Either[SimpleTypeElem, ComplexTypeElem]], openAttrs: Map[QName, String])

case class AnnotationElem(loc: Location, id: Option[String], seq: Seq[Either[AppInfoElem, DocumentationElem]], openAttrs: Map[QName, String]) extends OpenAttrs

case class AnyAttributeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], namespace: Option[NamespaceDefToken], notNamespace: Option[List[NamespaceItemToken]], processContent: Option[ProcessContentsToken], notQName: Option[List[QNameItemTokenA]], openAttrs: Map[QName, String]) extends NestedParticle

case class AnyElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], namespace: Option[NamespaceDefToken], notNamespace: Option[List[NamespaceItemToken]], processContent: Option[ProcessContentsToken], notQName: Option[List[QNameItemToken]], minOccurs: Option[Int], maxOccurs: Option[MaxOccursToken], openAttrs: Map[QName, String]) extends NestedParticle

case class AppInfoElem(loc: Location, source: Option[String], rawXml: String, openAttrs: Map[QName, String])

case class AttributeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], refType: Option[QName], use: Option[UseToken], default: Option[String], fixed: Option[String], form: Option[FormToken], targetNamespace: Option[URI], inheritable: Option[Boolean], simpleType: Option[SimpleTypeElem], openAttrs: Map[QName, String]) extends SchemaTopGroupElem

case class AttributeGroupDefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: String, attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem], openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class AttributeGroupRefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], ref: QName, openAttrs: Map[QName, String]) extends NestedParticle

case class ChoiceElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], particles: Seq[NestedParticle], openAttrs: Map[QName, String]) extends NestedParticle with InGroupDefParticle with TypeDefParticle

case class ComplexContentAbbrev(openContent: Option[OpenContentElem], typeDefParticle: Option[TypeDefParticle], attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], any: Option[AnyAttributeElem], asserts: Seq[AssertElem]) extends ComplexContent

case class ComplexContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], mixed: Option[Boolean], derivation: ComplexDerivation, openAttrs: Map[QName, String]) extends ComplexContent

case class ComplexExtensionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, openContent: Option[OpenContentElem], typeDefParticle: Option[TypeDefParticle], attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], any: Option[AnyAttributeElem], asserts: Seq[AssertElem], openAttrs: Map[QName, String]) extends ComplexDerivation

case class ComplexRestrictionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, openContent: Option[OpenContentElem], typeDefParticle: Option[TypeDefParticle], attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], any: Option[AnyAttributeElem], asserts: Seq[AssertElem], openAttrs: Map[QName, String]) extends ComplexDerivation

case class ComplexTypeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], mixed: Option[Boolean], abstrct: Option[Boolean], finl: Option[Derivation.CtrlSet[Derivation.CtFinalCtrl]], block: Option[Derivation.CtrlSet[Derivation.CtBlockCtrl]], defaultAttributesApply: Option[Boolean], content: ComplexTypeContent, openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class DefaultOpenContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], appliesToEmpty: Option[Boolean], mode: Option[DefaultOpenContentModeToken], any: Option[OpenContentAnyElem], openAttrs: Map[QName, String])

case class DocumentationElem(loc: Location, source: Option[String], lang: Option[String], rawXml: String, openAttrs: Map[QName, String])

case class ElementElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], refType: Option[QName], subsitutionGroup: Option[List[QName]], minOccurs: Option[Int], maxOccurs: Option[MaxOccursToken], default: Option[String], fixed: Option[String], nillable: Option[Boolean], abstr: Option[Boolean], finl: Option[Derivation.CtrlSet[Derivation.ElemFinalCtrl]], block: Option[Derivation.CtrlSet[Derivation.ElemBlockCtrl]], form: Option[FormToken], targetNamespace: Option[URI], inlinedType: Option[Either[SimpleTypeElem, ComplexTypeElem]], alternatives: Seq[AlternativeElem], constraints: Seq[IdentityConstraintGroupElem], openAttrs: Map[QName, String]) extends SchemaTopGroupElem with NestedParticle

case class FieldElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], xPath: String, xPathDefaultNamespace: Option[XPathDefaultNamespaceToken], openAttrs: Map[QName, String])

case class GroupDefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: String, particle: InGroupDefParticle, openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class GroupRefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], ref: QName, openAttrs: Map[QName, String]) extends NestedParticle with TypeDefParticle

case class ImportElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], schemaLocation: String, namespace: Option[String], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class IncludeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], schemaLocation: String, openAttrs: Map[QName, String]) extends CompositionGroupElem

case class KeyElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class KeyRefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], refer: Option[QName], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class ListElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], itemType: Option[QName], simpleType: Option[SimpleTypeElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

case class NotationElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: String, publ: Option[String], system: Option[URI], openAttrs: Map[QName, String]) extends SchemaTopGroupElem

case class OpenContentAnyElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], namespace: Option[NamespaceDefToken], notNamespace: Option[List[NamespaceItemToken]], processContent: Option[ProcessContentsToken], openAttrs: Map[QName, String])

case class OpenContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], mode: Option[OpenContentModeToken], any: Option[OpenContentAnyElem], openAttrs: Map[QName, String])

case class OverrideElem(loc: Location, id: Option[String], schemaLocation: String, overrides: Seq[Either[SchemaTopGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class RedefineElem(loc: Location, id: Option[String], schemaLocation: String, redefines: Seq[Either[RedefinableGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class SchemaElem(loc: Location, id: Option[String], compositions: Seq[Either[CompositionGroupElem, AnnotationElem]], defaultOpenContent: Option[DefaultOpenContentElem], schemaTop: Seq[Either[SchemaTopGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends OpenAttrs

case class SelectorElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], xPath: String, xPathDefaultNamespace: Option[XPathDefaultNamespaceToken], openAttrs: Map[QName, String])

case class SequenceElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], particles: Seq[NestedParticle], openAttrs: Map[QName, String]) extends NestedParticle with InGroupDefParticle with TypeDefParticle

case class SimpleContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], derivation: SimpleDerivation, openAttrs: Map[QName, String]) extends ComplexTypeContent

case class SimpleContentExtensionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], any: Option[AnyAttributeElem], asserts: Seq[AssertElem], openAttrs: Map[QName, String]) extends SimpleDerivation

case class SimpleContentRestrictionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, simpleType: Option[SimpleTypeElem], facets: Seq[FacetElem], attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], any: Option[AnyAttributeElem], asserts: Seq[AssertElem], openAttrs: Map[QName, String]) extends SimpleDerivation

case class SimpleTypeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], derivation: SimpleDerivationGroupElem, openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class SimpleTypeRestrictionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: Option[QName], tpe: Option[SimpleTypeElem], facets: Seq[FacetElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

case class UniqueElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class UnionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], memberTypes: Option[List[QName]], simpleTypes: Seq[SimpleTypeElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

//

case class AssertElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], test: Option[String], xPathDefaultNamespace: Option[XPathDefaultNamespaceToken], openAttrs: Map[QName, String]) extends FacetElem

case class ExplicitTimezoneElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: ExplicitTimezoneToken, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem

case class EnumerationElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, openAttrs: Map[QName, String]) extends FacetElem

case class FractionDigitsElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: Int, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem

case class LengthElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: Int, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem

case class MaxExclusiveElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem

case class MaxInclusiveElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem

case class MaxLengthElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: Int, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem

case class MinExclusiveElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem

case class MinInclusiveElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem

case class MinLengthElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: Int, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem

case class PatternElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, openAttrs: Map[QName, String]) extends FacetElem

case class TotalDigitsElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: Int, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem

case class WhitespaceElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: WhitespaceToken, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem

//

sealed trait XPathDefaultNamespaceToken

object XPathDefaultNamespaceToken {

  case object Default extends XPathDefaultNamespaceToken
  case object Target extends XPathDefaultNamespaceToken
  case object Local extends XPathDefaultNamespaceToken
  sealed case class AnyUri(uri: URI) extends XPathDefaultNamespaceToken

}

sealed trait MaxOccursToken

object MaxOccursToken {
  case object Unbounded extends MaxOccursToken
  case class Bounded(max: Int) extends MaxOccursToken
}

object Derivation {

  sealed trait Ctrl
  sealed trait ElemFinalCtrl extends Ctrl
  sealed trait ElemBlockCtrl extends Ctrl
  sealed trait CtFinalCtrl extends Ctrl
  sealed trait CtBlockCtrl extends Ctrl

  object Extension extends ElemFinalCtrl with ElemBlockCtrl with CtFinalCtrl with CtBlockCtrl
  object Restriction extends ElemFinalCtrl with ElemBlockCtrl with CtFinalCtrl with CtBlockCtrl
  object Substitution extends ElemBlockCtrl

  sealed trait CtrlSet[+C <: Ctrl]
  
  object All extends CtrlSet[Nothing]

  sealed case class CtrlExSet[C <: Ctrl](list: List[C]) extends CtrlSet[C]

}

sealed trait FormToken

object FormToken {
  object Qualified extends FormToken
  object Unqualified extends FormToken
}

sealed trait UseToken

object UseToken {
  object Optional extends UseToken
  object Required extends UseToken
  object Prohibited extends UseToken
}

sealed trait QNameItemToken // xs:qnameList

sealed trait QNameItemTokenA extends QNameItemToken // xs:qnameListA

object QNameItemToken {

  sealed case class Qn(qn: QName) extends QNameItemTokenA

  object Defined extends QNameItemTokenA
  object DefinedSibling extends QNameItemToken
}

sealed trait NamespaceItemToken

object NamespaceItemToken {
  object TargetNamespace extends NamespaceItemToken
  object Local extends NamespaceItemToken
  sealed case class Uri(uri: URI) extends NamespaceItemToken
}

sealed trait NamespaceDefToken

object NamespaceDefToken {
  object Any extends NamespaceDefToken
  object Other extends NamespaceDefToken
  sealed case class Items(list: List[NamespaceItemToken]) extends NamespaceDefToken
}

trait ProcessContentsToken

object ProcessContentsToken {
  object Skip extends ProcessContentsToken
  object Lax extends ProcessContentsToken
  object Strict extends ProcessContentsToken
}

sealed trait OpenContentModeToken

sealed trait DefaultOpenContentModeToken extends OpenContentModeToken

object OpenContentModeToken {
  object None extends OpenContentModeToken
  object Interleave extends DefaultOpenContentModeToken
  object Suffix extends DefaultOpenContentModeToken
}

trait WhitespaceToken

object WhitespaceToken {
  object Preserve extends WhitespaceToken
  object Replace extends WhitespaceToken
  object Collapse extends WhitespaceToken
}

trait ExplicitTimezoneToken

object ExplicitTimezoneToken {
  object Optional extends ExplicitTimezoneToken
  object Required extends ExplicitTimezoneToken
  object Prohibited extends ExplicitTimezoneToken
}