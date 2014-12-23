package eu.swdev.xml.xsd.cmp

import java.net.URI

import eu.swdev.etc.NotNothing
import eu.swdev.xml.base.{SomeValue, WhitespaceProcessing, Location}
import eu.swdev.xml.name.{Namespaces, QName}
import eu.swdev.xml.schema._

import scala.reflect.ClassTag
import scala.util.matching.Regex

/**
 *
 */

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

sealed trait FacetCmp

trait HasFacetSpecs {

  def facets: Seq[FacetElem]

  def facetSpecs: Seq[FacetCmp] = {
    val (fss, ees, pes) = facets.foldRight((List[FacetCmp](), List[EnumerationElem](), List[PatternElem]()))((i, acc) => i match {
      case fs: FacetCmp => (fs :: acc._1, acc._2, acc._3)
      case ee: EnumerationElem => (acc._1, ee :: acc._2, acc._3)
      case pe: PatternElem => (acc._1, acc._2, pe :: acc._3)
    })
    val t1 = if (ees.isEmpty) fss else EnumerationsFacetSpec(ees) :: fss
    if (pes.isEmpty) t1 else PatternsFacetSpec(pes) :: t1
  }
}

//

sealed trait ParticleCmp {
  def occurs: Occurs
}

sealed trait NestedParticleCmp extends ParticleCmp

sealed trait TypeDefParticleCmp extends ParticleCmp {
}

sealed trait GroupParticleCmp extends TypeDefParticleCmp {
  def nested: Seq[NestedParticleCmp]
}

//

sealed trait DerivationCmp {
  def base: QName
  def derivationMethod: CtDerivationCtrl
  def attrs: Seq[Either[AttributeElemL, AttributeGroupRefElem]]
  def anyAttribute: Option[AnyAttributeElem]
  def asserts: Seq[AssertElem]
}

sealed trait ComplexDerivationCmp extends DerivationCmp {
  def typeDefParticle: Option[TypeDefParticleCmp]
  def openContent: Option[OpenContentElem]
}

sealed trait SimpleDerivationCmp extends DerivationCmp {
}

sealed trait ComplexTypeContentCmp {
  def base: QName
  def derivationMethod: CtDerivationCtrl
}

sealed trait ComplexContentCmp extends ComplexTypeContentCmp {
  def loc: Location
  def mixed: Option[Boolean]
  def typeDefParticle: Option[TypeDefParticleCmp]
  def openContent: Option[OpenContentElem]
}

trait OpenContentCmp {
  def mode: SomeValue[OpenContentMode]
  def optAny: Option[OpenContentAnyElem]
}

trait WildcardCmp {
  def namespace: Option[NamespaceDefToken]
  def notNamespace: Option[List[NamespaceItemToken]]
  def processContents: SomeValue[ProcessContents]
  def notQName: Option[List[QNameItem]]
}

//
//
//

case class AllElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], occurs: Occurs, nested: Seq[NestedParticleCmp], openAttrs: Map[QName, String]) extends GroupParticleCmp

case class AlternativeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], test: Option[String], refType: Option[QName], xPathDefaultNamespace: Option[XPathDefaultNamespace], inlinedType: Option[Either[SimpleTypeElem, ComplexTypeElem]], openAttrs: Map[QName, String])

case class AnnotationElem(loc: Location, id: Option[String], seq: Seq[Either[AppInfoElem, DocumentationElem]], openAttrs: Map[QName, String])

case class AnyAttributeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], namespace: Option[NamespaceDefToken], notNamespace: Option[List[NamespaceItemToken]], processContents: SomeValue[ProcessContents], notQName: Option[List[QNameItemA]], openAttrs: Map[QName, String]) extends WildcardCmp

case class AnyElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], namespace: Option[NamespaceDefToken], notNamespace: Option[List[NamespaceItemToken]], processContents: SomeValue[ProcessContents], notQName: Option[List[QNameItem]], occurs: Occurs, openAttrs: Map[QName, String]) extends NestedParticleCmp with WildcardCmp

case class AppInfoElem(loc: Location, source: Option[String], rawXml: String, openAttrs: Map[QName, String])

sealed trait AttributeElem {
  def loc: Location
  def id: Option[String]
  def annotation: Option[AnnotationElem]
  def name: Option[String]
  def refType: Option[QName]
  def default: Option[String]
  def fixed: Option[String]
  def inheritable: Option[Boolean]
  def simpleType: Option[SimpleTypeElem]
}

case class AttributeElemG(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Some[String], refType: Option[QName], default: Option[String], fixed: Option[String], inheritable: Option[Boolean], simpleType: Option[SimpleTypeElem], openAttrs: Map[QName, String]) extends AttributeElem with SchemaTopGroupElem
case class AttributeElemL(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], refType: Option[QName], use: SomeValue[Use], default: Option[String], fixed: Option[String], form: Option[Form], targetNamespace: Option[URI], inheritable: Option[Boolean], simpleType: Option[SimpleTypeElem], openAttrs: Map[QName, String]) extends AttributeElem

case class AttributeGroupDefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: String, attrs: Seq[Either[AttributeElemL, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem], openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class AttributeGroupRefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], ref: QName, openAttrs: Map[QName, String])

case class ChoiceElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], occurs: Occurs, nested: Seq[NestedParticleCmp], openAttrs: Map[QName, String]) extends NestedParticleCmp with GroupParticleCmp

case class ComplexContentAbbrev(loc: Location, openContent: Option[OpenContentElem], typeDefParticle: Option[TypeDefParticleCmp], attrs: Seq[Either[AttributeElemL, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem], asserts: Seq[AssertElem]) extends ComplexContentCmp {
  override def base: QName = XsNames.ANY_TYPE
  override def derivationMethod = Relation.Restriction
  override def mixed: Option[Boolean] = None
}

case class ComplexContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], mixed: Option[Boolean], derivation: ComplexDerivationCmp, openAttrs: Map[QName, String]) extends ComplexContentCmp {
  override def base: QName = derivation.base
  override def derivationMethod = derivation.derivationMethod
  override def typeDefParticle: Option[TypeDefParticleCmp] = derivation.typeDefParticle
  override def openContent: Option[OpenContentElem] = derivation.openContent
}

case class ComplexExtensionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, openContent: Option[OpenContentElem], typeDefParticle: Option[TypeDefParticleCmp], attrs: Seq[Either[AttributeElemL, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem], asserts: Seq[AssertElem], openAttrs: Map[QName, String]) extends ComplexDerivationCmp {
  override def derivationMethod = Relation.Extension
}

case class ComplexRestrictionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, openContent: Option[OpenContentElem], typeDefParticle: Option[TypeDefParticleCmp], attrs: Seq[Either[AttributeElemL, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem], asserts: Seq[AssertElem], openAttrs: Map[QName, String]) extends ComplexDerivationCmp {
  override def derivationMethod = Relation.Restriction
}

case class ComplexTypeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: SomeValue[String], mixed: Option[Boolean], abstrct: SomeValue[Boolean], finl: Option[RelationSet[CtDerivationCtrl]], block: Option[RelationSet[CtBlockCtrl]], defaultAttributesApply: SomeValue[Boolean], content: ComplexTypeContentCmp, openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class DefaultOpenContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], appliesToEmpty: SomeValue[Boolean], mode: SomeValue[DefaultOpenContentMode], any: OpenContentAnyElem, openAttrs: Map[QName, String]) extends OpenContentCmp {
  override def optAny = Some(any)
}

case class DocumentationElem(loc: Location, source: Option[String], lang: Option[String], rawXml: String, openAttrs: Map[QName, String])

case class ElementElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], refType: Option[QName], subsitutionGroup: Option[List[QName]], occurs: Occurs, default: Option[String], fixed: Option[String], nillable: Option[Boolean], abstr: Option[Boolean], finl: Option[RelationSet[ElemFinalCtrl]], block: Option[RelationSet[ElemBlockCtrl]], form: Option[Form], targetNamespace: Option[URI], inlinedType: Option[Either[SimpleTypeElem, ComplexTypeElem]], alternatives: Seq[AlternativeElem], constraints: Seq[IdentityConstraintGroupElem], openAttrs: Map[QName, String]) extends SchemaTopGroupElem with NestedParticleCmp

case class FieldElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], xPath: String, xPathDefaultNamespace: Option[XPathDefaultNamespace], openAttrs: Map[QName, String])

case class GroupDefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: String, particle: GroupParticleCmp, openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class GroupRefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], ref: QName, occurs: Occurs, openAttrs: Map[QName, String]) extends NestedParticleCmp with TypeDefParticleCmp

case class ImportElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], schemaLocation: Option[String], namespace: Option[String], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class IncludeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], schemaLocation: String, openAttrs: Map[QName, String]) extends CompositionGroupElem

case class KeyElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class KeyRefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], refer: Option[QName], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class ListElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], itemType: Option[QName], simpleType: Option[SimpleTypeElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

case class NotationElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: String, publ: Option[String], system: Option[URI], openAttrs: Map[QName, String]) extends SchemaTopGroupElem

case class OpenContentAnyElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], namespace: Option[NamespaceDefToken], notNamespace: Option[List[NamespaceItemToken]], processContents: SomeValue[ProcessContents], openAttrs: Map[QName, String]) extends WildcardCmp {
  override def notQName: Option[List[QNameItem]] = None
}

case class OpenContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], mode: SomeValue[OpenContentMode], optAny: Option[OpenContentAnyElem], openAttrs: Map[QName, String]) extends OpenContentCmp

case class OverrideElem(loc: Location, id: Option[String], schemaLocation: String, overrides: Seq[Either[SchemaTopGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class RedefineElem(loc: Location, id: Option[String], schemaLocation: String, redefines: Seq[Either[RedefinableGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class SchemaElem(loc: Location, id: Option[String], targetNamespace: Option[URI], version: Option[String], finalDefault: SomeValue[RelationSet[TypeDerivationCtrl]], blockDefault: SomeValue[RelationSet[BlockCtrl]], attributeFormDefault: SomeValue[Form], elementFormDefault: SomeValue[Form], defaultAttributes: Option[QName], xPathDefaultNamespace: SomeValue[XPathDefaultNamespace], lang: Option[String], compositions: Seq[Either[CompositionGroupElem, AnnotationElem]], defaultOpenContent: Option[DefaultOpenContentElem], schemaTop: Seq[Either[SchemaTopGroupElem, AnnotationElem]], openAttrs: Map[QName, String])

case class SelectorElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], xPath: String, xPathDefaultNamespace: Option[XPathDefaultNamespace], openAttrs: Map[QName, String])

case class SequenceElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], occurs: Occurs, nested: Seq[NestedParticleCmp], openAttrs: Map[QName, String]) extends NestedParticleCmp with GroupParticleCmp

case class SimpleContentElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], derivation: SimpleDerivationCmp, openAttrs: Map[QName, String]) extends ComplexTypeContentCmp {
  override def base: QName = derivation.base
  def derivationMethod: CtDerivationCtrl = derivation.derivationMethod
}

case class SimpleContentExtensionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, attrs: Seq[Either[AttributeElemL, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem], asserts: Seq[AssertElem], openAttrs: Map[QName, String]) extends SimpleDerivationCmp {
  override def derivationMethod = Relation.Extension
}

case class SimpleContentRestrictionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: QName, simpleType: Option[SimpleTypeElem], facets: Seq[FacetElem], attrs: Seq[Either[AttributeElemL, AttributeGroupRefElem]], anyAttribute: Option[AnyAttributeElem], asserts: Seq[AssertElem], syntheticTypeName: String, openAttrs: Map[QName, String]) extends SimpleDerivationCmp with HasFacetSpecs {
  override def derivationMethod = Relation.Restriction
}

case class SimpleTypeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: SomeValue[String], finl: SomeValue[RelationSet[TypeDerivationCtrl]], derivation: SimpleDerivationGroupElem, openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class SimpleTypeRestrictionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: Option[QName], tpe: Option[SimpleTypeElem], facets: Seq[FacetElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem with HasFacetSpecs

case class UniqueElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class UnionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], memberTypes: Option[List[QName]], simpleTypes: Seq[SimpleTypeElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

//

case class AssertElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], test: String, xPathDefaultNamespace: Option[XPathDefaultNamespace], namespaces: Namespaces, openAttrs: Map[QName, String]) extends FacetElem with FacetCmp

case class ExplicitTimeZoneElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: ExplicitTimeZone, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem with FacetCmp

case class EnumerationElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, openAttrs: Map[QName, String], namespaces: Namespaces) extends FacetElem

case class FractionDigitsElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: Int, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem with FacetCmp

case class LengthElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: Int, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem with FacetCmp

case class MaxExclusiveElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, fixed: Option[Boolean], openAttrs: Map[QName, String], namespaces: Namespaces) extends FacetElem with FacetCmp

case class MaxInclusiveElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, fixed: Option[Boolean], openAttrs: Map[QName, String], namespaces: Namespaces) extends FacetElem with FacetCmp

case class MaxLengthElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: Int, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem with FacetCmp

case class MinExclusiveElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, fixed: Option[Boolean], openAttrs: Map[QName, String], namespaces: Namespaces) extends FacetElem with FacetCmp

case class MinInclusiveElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, fixed: Option[Boolean], openAttrs: Map[QName, String], namespaces: Namespaces) extends FacetElem with FacetCmp

case class MinLengthElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: Int, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem with FacetCmp

case class PatternElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, openAttrs: Map[QName, String]) extends FacetElem

case class TotalDigitsElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: Int, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem with FacetCmp

case class WhitespaceElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: WhitespaceProcessing, fixed: Option[Boolean], openAttrs: Map[QName, String]) extends FacetElem with FacetCmp

case class EnumerationsFacetSpec(value: Seq[EnumerationElem]) extends FacetCmp

case class PatternsFacetSpec(value: Seq[PatternElem]) extends FacetCmp

//

sealed trait NamespaceDefToken

object NamespaceDefToken {
  object Any extends NamespaceDefToken
  object Other extends NamespaceDefToken
  sealed case class Items(list: List[NamespaceItemToken]) extends NamespaceDefToken
}

sealed trait NamespaceItemToken

object NamespaceItemToken {
  object TargetNamespace extends NamespaceItemToken
  object Local extends NamespaceItemToken
  sealed case class Uri(uri: URI) extends NamespaceItemToken
}

sealed trait RelationSet[+R <: Relation]

object RelationSet {

  object All extends RelationSet[Nothing]

  def all[R <: Relation]: RelationSet[R] = All

  sealed case class Items[R <: Relation](list: List[R]) extends RelationSet[R]

  implicit class RelationSetOps[R <: Relation](rs: RelationSet[R]) {
    // require that type S is not inferred as being Nothing. This happens if
    def toSet[S <: R : ClassTag : NotNothing]: Set[S] = (rs match {
      case All => Relation.all
      case Items(list) => list
    }).collect { case s: S => s }.toSet
  }

}
