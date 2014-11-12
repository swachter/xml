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

sealed trait RedefinableGroupElem extends SchemaTopGroupElem

sealed trait SchemaTopGroupElem extends SchemaTopGroupOrAnnotationElem

sealed trait SchemaTopGroupOrAnnotationElem

//
//
//

case class AnnotationElem(loc: Location, openAttrs: Map[QName, String], id: Option[String], seq: Seq[AppInfoOrDocumentationElem]) extends OpenAttrs with CompositionGroupElem with SchemaTopGroupOrAnnotationElem

case class AppInfoElem(loc: Location, openAttrs: Map[QName, String], source: Option[String], rawXml: String) extends AppInfoOrDocumentationElem

case class DocumentationElem(loc: Location, openAttrs: Map[QName, String], source: Option[String], rawXml: String, lang: Option[String]) extends AppInfoOrDocumentationElem

case class SchemaElem(loc: Location, openAttrs: Map[QName, String], compositions: List[CompositionGroupElem], schemaTopOrAnnotationElem: List[SchemaTopGroupOrAnnotationElem]) extends OpenAttrs
