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

sealed trait AppInfoOrDocumentationElem extends OpenAttrs {
  def source: Option[String]
  def rawXml: String
}

sealed trait ComplexType extends Annotated

sealed trait Located {
  def loc: Location
}

sealed trait OpenAttrs extends Located {
  def openAttrs: Map[QName, String]
}

//
// Groups
//

sealed trait CompositionGroupElem

sealed trait SchemaTopGroupElem

sealed trait RedefinableGroupElem extends SchemaTopGroupElem


//
//
//

case class AnnotationElem(loc: Location, id: Option[String], seq: Seq[AppInfoOrDocumentationElem], openAttrs: Map[QName, String]) extends OpenAttrs

case class AppInfoElem(loc: Location, source: Option[String], rawXml: String, openAttrs: Map[QName, String]) extends AppInfoOrDocumentationElem

case class DocumentationElem(loc: Location, source: Option[String], lang: Option[String], rawXml: String, openAttrs: Map[QName, String]) extends AppInfoOrDocumentationElem

case class IncludeElem(loc: Location, id: Option[String], annotation: Option[AnnotationElem], schemaLocation: String, openAttrs: Map[QName, String]) extends CompositionGroupElem

case class RedefineElem(loc: Location, id: Option[String], schemaLocation: String, redefinables: Seq[Either[RedefinableGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends CompositionGroupElem

case class SchemaElem(loc: Location, id: Option[String], compositions: Seq[Either[CompositionGroupElem, AnnotationElem]], schemaTop: Seq[Either[SchemaTopGroupElem, AnnotationElem]], openAttrs: Map[QName, String]) extends OpenAttrs
