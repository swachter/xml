package eu.swdev.xml.xsd.parser

import javax.xml.stream.{XMLStreamConstants, XMLStreamReader}

import eu.swdev.xml.base.Location
import eu.swdev.xml.name.{Namespaces, LocalName, QNameFactory, QName, XsdNamespace, XmlNamespace}
import eu.swdev.xml.pushparser.XmlPushParserMod
import eu.swdev.xml.xsd.cmp._
import shapeless.ops.hlist.Prepend
import shapeless.{HList, Generic, HNil, ::}

trait XsdPushParserMod extends XmlPushParserMod {

  type OpenAttrsValue = Map[QName, String]
  type IdAttrValue = Option[String]

  lazy val annotated: Parser[IdAttrValue :: Option[AnnotationElem] :: HNil] = { val r = idAttr ~ annotation.opt; r }

  lazy val annotation: Parser[AnnotationElem] = xsElem("annotation")(idAttr ~ either(appInfo, documentation).rep) gmap Generic[AnnotationElem]

  lazy val appInfo: Parser[AppInfoElem] = xsElem("appinfo")(sourceAttr ~ rawXml) gmap Generic[AppInfoElem]

  lazy val baseAttr: Parser[IdAttrValue] = optionalAttr(QNameFactory.caching(new LocalName("base")))

  lazy val compositionGroupElem: Parser[CompositionGroupElem] = include | importElem | redefine | overrideElem

  lazy val documentation: Parser[DocumentationElem] = xsElem("documentation")(sourceAttr ~ langAttr ~ rawXml) gmap Generic[DocumentationElem]

  def either[L, R](l: Parser[L], r: Parser[R]): Parser[Either[L, R]] = (l map (Left(_))) | (r map (Right(_)))

  lazy val facet: Parser[FacetElem] = pattern // TODO

  lazy val idAttr: Parser[IdAttrValue] = optionalAttr(QNameFactory.caching(new LocalName("id")))

  lazy val importElem: Parser[ImportElem] = xsElem("import")(annotated ~ schemaLocationAttr ~ namespaceAttr) gmap Generic[ImportElem]

  lazy val include: Parser[IncludeElem] = xsElem("include")(annotated ~ schemaLocationAttr) gmap Generic[IncludeElem]

  lazy val langAttr = optionalAttr(QNameFactory.caching(XmlNamespace, new LocalName("lang")))

  lazy val nameAttr = optionalAttr(QNameFactory.caching(new LocalName("name")))

  lazy val namespaceAttr = optionalAttr(QNameFactory.caching(new LocalName("namespace")))

  lazy val noFixedFacet = annotated ~ valueAttr

  lazy val openAttrs: Parser[OpenAttrsValue] = selectAttrs(_.namespace != XsdNamespace)

  lazy val overrideElem: Parser[OverrideElem] = xsElem("override")(idAttr ~ schemaLocationAttr ~ either(schemaTopGroupElem, annotation).rep) gmap Generic[OverrideElem]

  lazy val pattern: Parser[PatternElem] = xsElem("pattern")(noFixedFacet) gmap Generic[PatternElem]

  lazy val redefinableGroupElem: Parser[RedefinableGroupElem] = simpleType // TODO | complexType | group | attributeGroup

  lazy val redefine: Parser[RedefineElem] = xsElem("redefine")(idAttr ~ schemaLocationAttr ~ either(redefinableGroupElem, annotation).rep) gmap Generic[RedefineElem]

  lazy val restriction: Parser[RestrictionElem] = xsElem("restriction")(annotated ~ baseAttr ~ simpleRestrictionModel) gmap Generic[RestrictionElem]

  lazy val schema: Parser[SchemaElem]  = xsElem("schema")(idAttr ~ either(compositionGroupElem, annotation).rep ~ either(schemaTopGroupElem, annotation).rep) gmap Generic[SchemaElem]

  lazy val schemaLocationAttr: Parser[String] = requiredAttr(QNameFactory.caching(new LocalName("schemaLocation")))

  lazy val schemaTopGroupElem: Parser[SchemaTopGroupElem] = redefinableGroupElem // TODO | element | attribute | notation

  lazy val simpleDerivationGroupElem: Parser[SimpleDerivationGroupElem] = restriction // TODO | list | union

  lazy val simpleRestrictionModel: Parser[Option[SimpleTypeElem] :: List[FacetElem] :: HNil] = { val r = simpleType.opt ~ facet.rep; r }

  lazy val simpleType: Parser[SimpleTypeElem] = xsElem("simpleType")(annotated ~ nameAttr ~ simpleDerivationGroupElem) gmap Generic[SimpleTypeElem]

  lazy val sourceAttr: Parser[Option[String]] = optionalAttr(QNameFactory.caching(new LocalName("source")))

  lazy val valueAttr: Parser[String] = requiredAttr(QNameFactory.caching(new LocalName("value")))

  //
  //
  //
  
  private implicit def xsdQName(localName: String): QName = QNameFactory.caching(XsdNamespace, new LocalName(localName))

  private lazy val openAttrsEndElem: Parser[OpenAttrsValue :: HNil] = { val r = openAttrs ~ endElement; r }

  private def xsElem[HL <: HList](name: String)(p: Parser[HL])(implicit ev: Prepend[Location :: HL, OpenAttrsValue :: HNil]) = startElement(name) ~ p ~ openAttrsEndElem


}
