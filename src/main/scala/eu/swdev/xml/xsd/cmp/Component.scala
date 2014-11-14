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

sealed trait CompositionElem

sealed trait SchemaTopElem

sealed trait RedefinableElem extends SchemaTopElem


//
//
//

case class AnnotationElem(loc: Location, id: Option[String], openAttrs: Map[QName, String], seq: Seq[AppInfoOrDocumentationElem]) extends OpenAttrs

case class AppInfoElem(loc: Location, source: Option[String], openAttrs: Map[QName, String], rawXml: String) extends AppInfoOrDocumentationElem

case class DocumentationElem(loc: Location, source: Option[String], lang: Option[String], openAttrs: Map[QName, String], rawXml: String) extends AppInfoOrDocumentationElem

case class IncludeElem(loc: Location, id: Option[String], openAttrs: Map[QName, String], annotation: Option[AnnotationElem], schemaLocation: String) extends CompositionElem

case class RedefineElem(loc: Location, id: Option[String], openAttrs: Map[QName, String], schemaLocation: String, redefinables: Seq[Either[RedefinableElem, AnnotationElem]]) extends CompositionElem

case class SchemaElem(loc: Location, id: Option[String], openAttrs: Map[QName, String], compositions: Seq[Either[CompositionElem, AnnotationElem]], schemaTop: Seq[Either[SchemaTopElem, AnnotationElem]]) extends OpenAttrs
