package eu.swdev.xml.xsd.parser

import javax.xml.stream.{XMLStreamConstants, XMLStreamReader}

import eu.swdev.xml.base.Location
import eu.swdev.xml.name._
import eu.swdev.xml.pushparser.XmlPushParserMod
import eu.swdev.xml.xsd.cmp.Derivation.ListedElementControls
import eu.swdev.xml.xsd.cmp._
import shapeless.ops.hlist.Prepend
import shapeless.{HList, Generic, HNil, ::}

trait XsdPushParserMod extends XmlPushParserMod {

  type OpenAttrsValue = Map[QName, String]
  type IdAttrValue = Option[String]

  lazy val abstractAttr = booleanAttr("abstract")

  lazy val alternative: Parser[AlternativeElem] = xsElem("alternative")(annotated ~ strAttr("test").opt ~ typeAttr.opt ~ xPathDefaultNamespaceAttr.opt ~ either(simpleType, complexType).opt) gmap Generic[AlternativeElem]

  lazy val annotated: Parser[IdAttrValue :: Option[AnnotationElem] :: HNil] = { val r = idAttr ~ annotation.opt; r }

  lazy val annotation: Parser[AnnotationElem] = xsElem("annotation")(idAttr ~ either(appInfo, documentation).rep) gmap Generic[AnnotationElem]

  lazy val appInfo: Parser[AppInfoElem] = xsElem("appinfo")(sourceAttr ~ rawXml) gmap Generic[AppInfoElem]

  lazy val baseAttr: Parser[QName] = qnAttr("base")

  lazy val complexType: Parser[Nothing] = ??? // TODO

  lazy val compositionGroupElem: Parser[CompositionGroupElem] = include | importElem | redefine | overrideElem

  lazy val defaultAttr = strAttr("default")

  lazy val defRef = nameAttr.opt ~ refAttr.opt

  lazy val documentation: Parser[DocumentationElem] = xsElem("documentation")(sourceAttr ~ langAttr ~ rawXml) gmap Generic[DocumentationElem]

  lazy val element: Parser[ElementElem] = xsElem("element")(annotated ~ defRef ~ typeAttr.opt ~ substitutionGroup.opt ~ occurs ~ defaultAttr.opt ~ fixedAttr.opt ~ nillableAttr.opt ~ abstractAttr.opt ~ finalOnElementAttr.opt ~ either(simpleType, complexType).opt ~ alternative.rep ~ identityConstraint.rep) gmap Generic[ElementElem ]

  lazy val facet: Parser[FacetElem] = pattern // TODO

  lazy val field: Parser[FieldElem] = xsElem("field")(annotated ~ xPathAttr ~ xPathDefaultNamespaceAttr.opt) gmap Generic[FieldElem]

  lazy val finalOnElementAttr: Parser[Derivation.ElementControlSet] = strAttr("final") flatMap {
    case "#all" => success(Derivation.All)
    case s => {
      Parser.traverse(s.split("\\s+").toList){
        case "extension" => success(Derivation.Extension)
        case "restriction" => success(Derivation.Restriction)
        case x => fail(s"illegal final item: $x")
      }.map(ListedElementControls(_))
    }
  }

  lazy val fixedAttr = strAttr("fixed")

  lazy val idAttr: Parser[IdAttrValue] = strAttr("id").opt

  lazy val identityConstraint: Parser[IdentityConstraintGroupElem] = unique | key | keyref

  lazy val importElem: Parser[ImportElem] = xsElem("import")(annotated ~ schemaLocationAttr ~ namespaceAttr) gmap Generic[ImportElem]

  lazy val include: Parser[IncludeElem] = xsElem("include")(annotated ~ schemaLocationAttr) gmap Generic[IncludeElem]

  lazy val itemTypeAttr: Parser[QName] = qnAttr("itemType")

  lazy val key: Parser[KeyElem] = xsElem("key")(keybase) gmap Generic[KeyElem]

  lazy val keybase = annotated ~ strAttr("name").opt ~ qnAttr("ref").opt ~ (selector & field.rep1).opt

  lazy val keyref: Parser[KeyRefElem] = xsElem("keyref")(keybase ~ qnAttr("refer").opt) gmap Generic[KeyRefElem]

  lazy val langAttr = optionalAttr(QNameFactory.caching(XmlNamespace, new LocalName("lang")))

  lazy val list: Parser[ListElem] = xsElem("list")(annotated ~ itemTypeAttr.opt ~ simpleType.opt) gmap Generic[ListElem]

  lazy val maxOccursAttr: Parser[MaxOccursToken] = (strAttr("maxOccurs") map {
    case "unbounded" => Unbounded
    case s => MaxBounded(s.toInt)

  }) | success(MaxBounded(1))

  lazy val memberTypes: Parser[List[QName]] = qNamesParser(strAttr("memberTypes"))

  lazy val minOccursAttr: Parser[Int] = (strAttr("minOccurs") map (_.toInt)) | success(1)

  lazy val nameAttr = strAttr("name")

  lazy val namespaceAttr = optionalAttr(QNameFactory.caching(new LocalName("namespace")))

  lazy val nillableAttr = booleanAttr("nillable")

  lazy val noFixedFacet = annotated ~ valueAttr

  lazy val occurs = minOccursAttr ~ maxOccursAttr

  lazy val openAttrs: Parser[OpenAttrsValue] = selectAttrs(_.namespace != XsdNamespace)

  lazy val overrideElem: Parser[OverrideElem] = xsElem("override")(idAttr ~ schemaLocationAttr ~ either(schemaTopGroupElem, annotation).rep) gmap Generic[OverrideElem]

  lazy val pattern: Parser[PatternElem] = xsElem("pattern")(noFixedFacet) gmap Generic[PatternElem]

  lazy val redefinableGroupElem: Parser[RedefinableGroupElem] = simpleType | complexType // TODO | group | attributeGroup

  lazy val redefine: Parser[RedefineElem] = xsElem("redefine")(idAttr ~ schemaLocationAttr ~ either(redefinableGroupElem, annotation).rep) gmap Generic[RedefineElem]

  lazy val refAttr = qnAttr("ref")

  lazy val restriction: Parser[RestrictionElem] = xsElem("restriction")(annotated ~ baseAttr.opt ~ simpleRestrictionModel) gmap Generic[RestrictionElem]

  lazy val schema: Parser[SchemaElem]  = xsElem("schema")(idAttr ~ either(compositionGroupElem, annotation).rep ~ either(schemaTopGroupElem, annotation).rep) gmap Generic[SchemaElem]

  lazy val schemaLocationAttr: Parser[String] = requiredAttr(QNameFactory.caching(new LocalName("schemaLocation")))

  lazy val schemaTopGroupElem: Parser[SchemaTopGroupElem] = redefinableGroupElem | element // TODO | attribute | notation

  lazy val selector: Parser[SelectorElem] = xsElem("selector")(annotated ~ xPathAttr ~ xPathDefaultNamespaceAttr.opt) gmap Generic[SelectorElem]

  lazy val simpleDerivationGroupElem: Parser[SimpleDerivationGroupElem] = restriction | list | union

  lazy val simpleRestrictionModel: Parser[Option[SimpleTypeElem] :: List[FacetElem] :: HNil] = { val r = simpleType.opt ~ facet.rep; r }

  lazy val simpleType: Parser[SimpleTypeElem] = xsElem("simpleType")(annotated ~ nameAttr.opt ~ simpleDerivationGroupElem) gmap Generic[SimpleTypeElem]

  lazy val sourceAttr: Parser[Option[String]] = optionalAttr(QNameFactory.caching(new LocalName("source")))

  lazy val substitutionGroup: Parser[List[QName]] = qNamesParser(strAttr("substitutionGroup"))

  lazy val typeAttr = qnAttr("type")

  lazy val union: Parser[UnionElem] = xsElem("union")(annotated ~ memberTypes.opt ~ simpleType.rep) gmap Generic[UnionElem]

  lazy val unique: Parser[UniqueElem] = xsElem("unique")(keybase) gmap Generic[UniqueElem]

  lazy val valueAttr: Parser[String] = requiredAttr(QNameFactory.caching(new LocalName("value")))

  lazy val xPathAttr: Parser[String] = strAttr("xpath")

  lazy val xPathDefaultNamespaceAttr: Parser[NamespaceToken.XPathDefault] = strAttr("xpathDefaultNamespace") map {
    case "##defaultNamespace" => NamespaceToken.Default
    case "##targetNamespace" => NamespaceToken.Target
    case "##local" => NamespaceToken.Local
    case s => NamespaceToken.AnyUri(s)
  }

  //
  //
  //
  
  private lazy val openAttrsEndElem: Parser[OpenAttrsValue :: HNil] = { val r = openAttrs ~ endElement; r }

  def either[L, R](l: Parser[L], r: => Parser[R]): Parser[Either[L, R]] = (l map (Left(_))) | (r map (Right(_)))

  private def xsElem[HL <: HList](name: String)(p: => Parser[HL])(implicit ev: Prepend[Location :: HL, OpenAttrsValue :: HNil]) = startElement(QNameFactory.caching(XsdNamespace, new LocalName(name))) ~ p ~ openAttrsEndElem

  private def strAttr(name: String): Parser[String] = requiredAttr(QNameFactory.caching(new LocalName(name)))

  private def qnAttr(name: String): Parser[QName] = strAttr(name) flatMap resolveQn

  private def qNamesParser(p: Parser[String]): Parser[List[QName]] = p map (_.split("\\s+").toList) flatMap (Parser.traverse(_)(resolveQn))

  private def booleanAttr(name: String): Parser[Boolean] = strAttr(name) flatMap {
    case "true" => success(true)
    case "1" => success(true)
    case "false" => success(false)
    case "0" => success(false)
    case s => fail(s"illegal boolean: $s")
  }

}
