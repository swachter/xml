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

case class AlternativeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], test: Option[String], refType: Option[QName], xPathDefaultNamespace: Option[NamespaceToken.XPathDefault], inlinedType: Option[Either[SimpleTypeElem, ComplexTypeElem]], openAttrs: Map[QName, String])

case class AnnotationElem(loc: Location, id: Option[String], seq: Seq[Either[AppInfoElem, DocumentationElem]], openAttrs: Map[QName, String]) extends OpenAttrs

case class AnyAttributeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], namespace: Option[NsList.Def], notNamespace: Option[List[NsList.Token]], processContent: Option[Pc.Token], notQName: Option[List[QnList.TokenA]], openAttrs: Map[QName, String]) extends NestedParticle

case class AnyElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], namespace: Option[NsList.Def], notNamespace: Option[List[NsList.Token]], processContent: Option[Pc.Token], notQName: Option[List[QnList.Token]], minOccurs: Option[Int], maxOccurs: Option[MaxOccursToken], openAttrs: Map[QName, String]) extends NestedParticle

case class AppInfoElem(loc: Location, source: Option[String], rawXml: String, openAttrs: Map[QName, String])

case class AssertElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], test: Option[String], xPathDefaultNamespace: Option[NamespaceToken.XPathDefault], openAttrs: Map[QName, String])

case class AttributeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], refType: Option[QName], use: Option[Use], default: Option[String], fixed: Option[String], form: Option[Form], targetNamespace: Option[URI], inheritable: Option[Boolean], simpleType: Option[SimpleTypeElem], openAttrs: Map[QName, String]) extends SchemaTopGroupElem

case class AttributeGroupDefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: String, attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem], openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class AttributeGroupRefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], ref: QName, openAttrs: Map[QName, String]) extends NestedParticle

case class ChoiceElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], particles: Seq[NestedParticle], openAttrs: Map[QName, String]) extends NestedParticle with InGroupDefParticle with TypeDefParticle

case class ComplexContentAbbrev(openContent: Option[OpenContentElem], typeDefParticle: Option[TypeDefParticle], attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], any: Option[AnyAttributeElem], asserts: Seq[AssertElem]) extends ComplexContent

case class ComplexContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], mixed: Option[Boolean], derivation: ComplexDerivation, openAttrs: Map[QName, String]) extends ComplexContent

case class ComplexExtensionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, openContent: Option[OpenContentElem], typeDefParticle: Option[TypeDefParticle], attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], any: Option[AnyAttributeElem], asserts: Seq[AssertElem], openAttrs: Map[QName, String]) extends ComplexDerivation

case class ComplexRestrictionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, openContent: Option[OpenContentElem], typeDefParticle: Option[TypeDefParticle], attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], any: Option[AnyAttributeElem], asserts: Seq[AssertElem], openAttrs: Map[QName, String]) extends ComplexDerivation

case class ComplexTypeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], mixed: Option[Boolean], abstrct: Option[Boolean], finl: Option[Derivation.CtrlSet[Derivation.CtFinalCtrl]], block: Option[Derivation.CtrlSet[Derivation.CtBlockCtrl]], defaultAttributesApply: Option[Boolean], content: ComplexTypeContent, openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class DefaultOpenContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], appliesToEmpty: Option[Boolean], mode: Option[DefaultOpenContentMode], any: Option[OpenContentAnyElem], openAttrs: Map[QName, String])

case class DocumentationElem(loc: Location, source: Option[String], lang: Option[String], rawXml: String, openAttrs: Map[QName, String])

case class ElementElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], refType: Option[QName], subsitutionGroup: Option[List[QName]], minOccurs: Option[Int], maxOccurs: Option[MaxOccursToken], default: Option[String], fixed: Option[String], nillable: Option[Boolean], abstr: Option[Boolean], finl: Option[Derivation.CtrlSet[Derivation.ElemFinalCtrl]], block: Option[Derivation.CtrlSet[Derivation.ElemBlockCtrl]], form: Option[Form], targetNamespace: Option[URI], inlinedType: Option[Either[SimpleTypeElem, ComplexTypeElem]], alternatives: Seq[AlternativeElem], constraints: Seq[IdentityConstraintGroupElem], openAttrs: Map[QName, String]) extends SchemaTopGroupElem with NestedParticle

case class FieldElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], xPath: String, xPathDefaultNamespace: Option[NamespaceToken.XPathDefault], openAttrs: Map[QName, String])

case class GroupDefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: String, particle: InGroupDefParticle, openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class GroupRefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], ref: QName, openAttrs: Map[QName, String]) extends NestedParticle with TypeDefParticle

case class ImportElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], schemaLocation: String, namespace: Option[String], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class IncludeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], schemaLocation: String, openAttrs: Map[QName, String]) extends CompositionGroupElem

case class KeyElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class KeyRefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], refer: Option[QName], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class ListElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], itemType: Option[QName], simpleType: Option[SimpleTypeElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

case class NotationElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: String, publ: Option[String], system: Option[URI], openAttrs: Map[QName, String]) extends SchemaTopGroupElem

case class OpenContentAnyElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], namespace: Option[NsList.Def], notNamespace: Option[List[NsList.Token]], processContent: Option[Pc.Token], openAttrs: Map[QName, String])

case class OpenContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], mode: Option[OpenContentMode], any: Option[OpenContentAnyElem], openAttrs: Map[QName, String])

case class OverrideElem(loc: Location, id: Option[String], schemaLocation: String, overrides: Seq[Either[SchemaTopGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class PatternElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, openAttrs: Map[QName, String]) extends FacetElem

case class RedefineElem(loc: Location, id: Option[String], schemaLocation: String, redefines: Seq[Either[RedefinableGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class SchemaElem(loc: Location, id: Option[String], compositions: Seq[Either[CompositionGroupElem, AnnotationElem]], defaultOpenContent: Option[DefaultOpenContentElem], schemaTop: Seq[Either[SchemaTopGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends OpenAttrs

case class SelectorElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], xPath: String, xPathDefaultNamespace: Option[NamespaceToken.XPathDefault], openAttrs: Map[QName, String])

case class SequenceElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], particles: Seq[NestedParticle], openAttrs: Map[QName, String]) extends NestedParticle with InGroupDefParticle with TypeDefParticle

case class SimpleContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], derivation: SimpleDerivation, openAttrs: Map[QName, String]) extends ComplexTypeContent

case class SimpleContentExtensionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], any: Option[AnyAttributeElem], asserts: Seq[AssertElem], openAttrs: Map[QName, String]) extends SimpleDerivation

case class SimpleContentRestrictionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, simpleType: Option[SimpleTypeElem], facets: Seq[FacetElem], attrs: Seq[Either[AttributeElem, AttributeGroupRefElem]], any: Option[AnyAttributeElem], asserts: Seq[AssertElem], openAttrs: Map[QName, String]) extends SimpleDerivation

case class SimpleTypeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], derivation: SimpleDerivationGroupElem, openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class SimpleTypeRestrictionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: Option[QName], tpe: Option[SimpleTypeElem], facets: Seq[FacetElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

case class UniqueElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class UnionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], memberTypes: Option[List[QName]], simpleTypes: Seq[SimpleTypeElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

//

object NamespaceToken {

  sealed trait XPathDefault

  case object Default extends XPathDefault
  case object Target extends XPathDefault
  case object Local extends XPathDefault
  sealed case class AnyUri(uri: URI) extends XPathDefault

}

sealed trait MaxOccursToken

case object Unbounded extends MaxOccursToken
case class MaxBounded(max: Int) extends MaxOccursToken

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

sealed trait Form

object Qualified extends Form
object Unqualified extends Form

sealed trait Use

object Optional extends Use
object Required extends Use
object Prohibited extends Use

object QnList {

  sealed trait Token // xs:qnameList

  sealed trait TokenA extends Token // xs:qnameListA

  sealed case class QnItem(qn: QName) extends TokenA

  object Defined extends TokenA
  object DefinedSibling extends Token
}

object NsList {

  sealed trait Token

  object TargetNamespace extends Token
  object Local extends Token

  sealed case class UriItem(uri: URI) extends Token
  
  sealed trait Def
  object Any extends Def
  object Other extends Def
  sealed case class ExDef(list: List[Token]) extends Def
}

object Pc {
  trait Token
  object Skip extends Token
  object Lax extends Token
  object Strict extends Token
}

sealed trait OpenContentMode

sealed trait DefaultOpenContentMode extends OpenContentMode

object OpenContentMode {
  object None extends OpenContentMode
  object Interleave extends DefaultOpenContentMode
  object Suffix extends DefaultOpenContentMode
}