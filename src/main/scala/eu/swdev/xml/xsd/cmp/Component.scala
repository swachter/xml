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

sealed trait RedefinableGroupElem extends SchemaTopGroupElem

sealed trait SchemaTopGroupElem

sealed trait SimpleDerivationGroupElem

//
//
//

case class AnnotationElem(loc: Location, id: Option[String], seq: Seq[Either[AppInfoElem, DocumentationElem]], openAttrs: Map[QName, String]) extends OpenAttrs

case class AppInfoElem(loc: Location, source: Option[String], rawXml: String, openAttrs: Map[QName, String])

case class DocumentationElem(loc: Location, source: Option[String], lang: Option[String], rawXml: String, openAttrs: Map[QName, String])

case class ImportElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], schemaLocation: String, namespace: Option[String], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class IncludeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], schemaLocation: String, openAttrs: Map[QName, String]) extends CompositionGroupElem

case class ListElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], itemType: Option[QName], simpleType: Option[SimpleTypeElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

case class OverrideElem(loc: Location, id: Option[String], schemaLocation: String, overrides: Seq[Either[SchemaTopGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class PatternElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], value: String, openAttrs: Map[QName, String]) extends FacetElem

case class RedefineElem(loc: Location, id: Option[String], schemaLocation: String, redefines: Seq[Either[RedefinableGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class RestrictionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], base: Option[QName], tpe: Option[SimpleTypeElem], facets: Seq[FacetElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

case class SchemaElem(loc: Location, id: Option[String], compositions: Seq[Either[CompositionGroupElem, AnnotationElem]], schemaTop: Seq[Either[SchemaTopGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends OpenAttrs

case class SimpleTypeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], name: Option[String], derivation: SimpleDerivationGroupElem, openAttrs: Map[QName, String]) extends RedefinableGroupElem

case class UnionElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], memberTypes: Option[List[QName]], simpleTypes: Seq[SimpleTypeElem], openAttrs: Map[QName, String]) extends SimpleDerivationGroupElem

