package eu.swdev.xml.xsd.cmp

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
//
//

case class AlternativeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], test: Option[String], refType: Option[QName], xPathDefaultNamespace: Option[NamespaceToken.XPathDefault], inlinedType: Option[Either[SimpleTypeElem, ComplexTypeElem]], openAttrs: Map[QName, String])

case class AnnotationElem(loc: Location, id: Option[String], seq: Seq[Either[AppInfoElem, DocumentationElem]], openAttrs: Map[QName, String]) extends OpenAttrs

case class AppInfoElem(loc: Location, source: Option[String], rawXml: String, openAttrs: Map[QName, String])

case class ComplexTypeElem()

case class DocumentationElem(loc: Location, source: Option[String], lang: Option[String], rawXml: String, openAttrs: Map[QName, String])

case class ElementElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], refType: Option[QName], subsitutionGroup: Option[List[QName]], minOccurs: Int, maxOccurs: MaxOccursToken, inlinedType: Option[Either[SimpleTypeElem, ComplexTypeElem]], alternatives: Seq[AlternativeElem], constraints: Seq[IdentityConstraintGroupElem], openAttrs: Map[QName, String]) extends SchemaTopGroupElem

case class FieldElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], xPath: String, xPathDefaultNamespace: Option[NamespaceToken.XPathDefault], openAttrs: Map[QName, String])

case class ImportElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], schemaLocation: String, namespace: Option[String], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class IncludeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], schemaLocation: String, openAttrs: Map[QName, String]) extends CompositionGroupElem

case class KeyElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class KeyRefElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], refer: Option[QName], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class ListElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], itemType: Option[QName], simpleType: Option[SimpleTypeElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

case class OverrideElem(loc: Location, id: Option[String], schemaLocation: String, overrides: Seq[Either[SchemaTopGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class PatternElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, openAttrs: Map[QName, String]) extends FacetElem

case class RedefineElem(loc: Location, id: Option[String], schemaLocation: String, redefines: Seq[Either[RedefinableGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class RestrictionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: Option[QName], tpe: Option[SimpleTypeElem], facets: Seq[FacetElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

case class SchemaElem(loc: Location, id: Option[String], compositions: Seq[Either[CompositionGroupElem, AnnotationElem]], schemaTop: Seq[Either[SchemaTopGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends OpenAttrs

case class SelectorElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], xPath: String, xPathDefaultNamespace: Option[NamespaceToken.XPathDefault], openAttrs: Map[QName, String])

case class SimpleTypeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], derivation: SimpleDerivationGroupElem, openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class UniqueElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], ref: Option[QName], sel: Option[(SelectorElem, List[FieldElem])], openAttrs: Map[QName, String]) extends IdentityConstraintGroupElem

case class UnionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], memberTypes: Option[List[QName]], simpleTypes: Seq[SimpleTypeElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

//

object NamespaceToken {

  sealed trait XPathDefault

  case object Default extends XPathDefault
  case object Target extends XPathDefault
  case object Local extends XPathDefault
  sealed case class AnyUri(string: String) extends XPathDefault

}

sealed trait MaxOccursToken

case object Unbounded extends MaxOccursToken
case class MaxBounded(max: Int) extends MaxOccursToken
