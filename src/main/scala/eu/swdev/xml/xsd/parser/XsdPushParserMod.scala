package eu.swdev.xml.xsd.parser

import java.net.URI

import eu.swdev.xml.base.Location
import eu.swdev.xml.name._
import eu.swdev.xml.pushparser.XmlPushParserMod
import eu.swdev.xml.xsd.cmp._
import shapeless.ops.hlist.Prepend
import shapeless.{::, Generic, HList, HNil}

import scala.util.{Failure, Success, Try}

trait XsdPushParserMod extends XmlPushParserMod {

  type OpenAttrsValue = Map[QName, String]
  type IdAttrValue = Option[String]

  lazy val abstractAttr = booleanAttr("abstract")

  lazy val all: Parser[AllElem] = xsElem("all")(annotated ~ nestedParticle.rep) gmap Generic[AllElem]

  lazy val alternative: Parser[AlternativeElem] = xsElem("alternative")(annotated ~ testAttr.opt ~ typeAttr.opt ~ xPathDefaultNamespaceAttr.opt ~ either(simpleType, complexType).opt) gmap Generic[AlternativeElem]

  lazy val annotated: Parser[IdAttrValue :: Option[AnnotationElem] :: HNil] = { val r = idAttr ~ annotation.opt; r }

  lazy val annotation: Parser[AnnotationElem] = xsElem("annotation")(idAttr ~ either(appInfo, documentation).rep) gmap Generic[AnnotationElem]

  lazy val any: Parser[AnyElem] = xsElem("any")(annotated ~ anyAttrGroup ~ notQNameAttr.opt ~ occurs) gmap Generic[AnyElem]

  lazy val anyAttrGroup = namespaceListAttr.opt ~ notNamespaceAttr.opt ~ processContentsAttr.opt

  lazy val anyAttribute: Parser[AnyAttributeElem] = xsElem("anyAttribute")(annotated ~ anyAttrGroup ~ notQNameAttrA.opt) gmap Generic[AnyAttributeElem]

  lazy val appInfo: Parser[AppInfoElem] = xsElem("appinfo")(sourceAttr ~ rawXml) gmap Generic[AppInfoElem]

  lazy val assert: Parser[AssertElem] = xsElem("assert")(annotated ~ testAttr.opt ~ xPathDefaultNamespaceAttr.opt) gmap Generic[AssertElem]

  lazy val attrDecls = either(attribute, attributeGroupRef).rep ~ anyAttribute.opt

  lazy val attribute: Parser[AttributeElem] = xsElem("attribute")(annotated ~ defRef ~ typeAttr.opt ~ useAttr.opt ~ defaultAttr.opt ~ fixedAttr.opt ~ formAttr.opt ~ targetNamespaceAttr.opt ~ inheritableAttr.opt ~ simpleType.opt) gmap Generic[AttributeElem]
  
  lazy val attributeGroupDef: Parser[AttributeGroupDefElem] = xsElem("attributeGroup")(annotated ~ nameAttr ~ attrDecls) gmap Generic[AttributeGroupDefElem]

  lazy val attributeGroupRef: Parser[AttributeGroupRefElem] = xsElem("attributeGroup")(annotated ~ refAttr) gmap Generic[AttributeGroupRefElem]

  lazy val baseAttr: Parser[QName] = qnAttr("base")

  lazy val choice: Parser[ChoiceElem] = xsElem("choice")(annotated ~ nestedParticle.rep) gmap Generic[ChoiceElem]

  lazy val complexContent: Parser[ComplexContentElem] = xsElem("complexContent")(annotated ~ mixedAttr.opt ~ (complexRestriction | complexExtension)) gmap Generic[ComplexContentElem]
  
  lazy val complexContentAbbrev: Parser[ComplexContentAbbrev] = complexContentModel gmap Generic[ComplexContentAbbrev]

  lazy val complexContentModel = openContent.opt ~ typeDefParticle.opt ~ attrDecls ~ assert.rep

  lazy val complexExtension: Parser[ComplexExtensionElem] = xsElem("extension")(annotated ~ baseAttr ~ complexContentModel) gmap Generic[ComplexExtensionElem]

  lazy val complexRestriction: Parser[ComplexRestrictionElem] = xsElem("restriction")(annotated ~ baseAttr ~ complexContentModel) gmap Generic[ComplexRestrictionElem]

  lazy val complexType: Parser[ComplexTypeElem] = xsElem("complexType")(annotated ~ nameAttr.opt ~ mixedAttr.opt ~ abstractAttr.opt ~ complexTypeFinalAttr.opt ~ complexTypeBlockAttr.opt ~ defaultAttributesApplyAttr.opt ~ complexTypeContent) gmap Generic[ComplexTypeElem]

  lazy val complexTypeBlockAttr: Parser[Derivation.CtrlSet[Derivation.CtBlockCtrl]] = derivationCtrlAttr("block")(dcExtension orElse dcRestriction)

  lazy val complexTypeFinalAttr: Parser[Derivation.CtrlSet[Derivation.CtFinalCtrl]] = derivationCtrlAttr("final")(dcExtension orElse dcRestriction)

  lazy val complexTypeContent: Parser[ComplexTypeContent] = simpleContent | complexContent | complexContentAbbrev

  lazy val compositionGroupElem: Parser[CompositionGroupElem] = include | importElem | redefine | overrideElem

  lazy val defaultAttr: Parser[String] = strAttr("default")

  lazy val defaultAttributesApplyAttr: Parser[Boolean] = booleanAttr("defaultAttributesApply")

  lazy val defRef = nameAttr.opt ~ refAttr.opt

  lazy val documentation: Parser[DocumentationElem] = xsElem("documentation")(sourceAttr ~ langAttr ~ rawXml) gmap Generic[DocumentationElem]

  lazy val element: Parser[ElementElem] = xsElem("element")(annotated ~ defRef ~ typeAttr.opt ~ substitutionGroup.opt ~ occurs ~ defaultAttr.opt ~ fixedAttr.opt ~ nillableAttr.opt ~ abstractAttr.opt ~ elementFinalAttr.opt ~ elementBlockAttr.opt ~ formAttr.opt ~ targetNamespaceAttr.opt ~ either(simpleType, complexType).opt ~ alternative.rep ~ identityConstraint.rep) gmap Generic[ElementElem ]

  lazy val elementBlockAttr: Parser[Derivation.CtrlSet[Derivation.ElemBlockCtrl]] = derivationCtrlAttr("block")(dcExtension orElse dcRestriction orElse dcSubstitution)

  lazy val elementFinalAttr: Parser[Derivation.CtrlSet[Derivation.ElemFinalCtrl]] = derivationCtrlAttr("final")(dcExtension orElse dcRestriction)

  lazy val facet: Parser[FacetElem] = pattern // TODO

  lazy val field: Parser[FieldElem] = xsElem("field")(annotated ~ xPathAttr ~ xPathDefaultNamespaceAttr.opt) gmap Generic[FieldElem]

  lazy val fixedAttr = strAttr("fixed")

  lazy val formAttr = strAttr("form") >>= {
    case "qualified" => success(Qualified)
    case "unqualified" => success(Unqualified)
    case s => fail(s"invalid form: $s")
  }

  lazy val groupDef: Parser[GroupDefElem] = xsElem("group")(annotated ~ nameAttr ~ (all | choice | sequence)) gmap Generic[GroupDefElem]

  lazy val groupRef: Parser[GroupRefElem] = xsElem("group")(annotated ~ refAttr) gmap Generic[GroupRefElem]

  lazy val idAttr: Parser[IdAttrValue] = strAttr("id").opt

  lazy val identityConstraint: Parser[IdentityConstraintGroupElem] = unique | key | keyref

  lazy val importElem: Parser[ImportElem] = xsElem("import")(annotated ~ schemaLocationAttr ~ namespaceAttr) gmap Generic[ImportElem]

  lazy val include: Parser[IncludeElem] = xsElem("include")(annotated ~ schemaLocationAttr) gmap Generic[IncludeElem]

  lazy val inheritableAttr: Parser[Boolean] = booleanAttr("inheritable")

  lazy val itemTypeAttr: Parser[QName] = qnAttr("itemType")

  lazy val key: Parser[KeyElem] = xsElem("key")(keybase) gmap Generic[KeyElem]

  lazy val keybase = annotated ~ strAttr("name").opt ~ qnAttr("ref").opt ~ (selector & field.rep1).opt

  lazy val keyref: Parser[KeyRefElem] = xsElem("keyref")(keybase ~ qnAttr("refer").opt) gmap Generic[KeyRefElem]

  lazy val langAttr = optionalAttr(QNameFactory.caching(XmlNamespace, new LocalName("lang")))

  lazy val list: Parser[ListElem] = xsElem("list")(annotated ~ itemTypeAttr.opt ~ simpleType.opt) gmap Generic[ListElem]

  lazy val maxOccursAttr: Parser[MaxOccursToken] = strAttr("maxOccurs") >>= {
    case "unbounded" => success(Unbounded)
    case s => parseInt.apply(s).map(MaxBounded(_))
  }

  lazy val memberTypes: Parser[List[QName]] = qNamesAttr("memberTypes")

  lazy val mixedAttr: Parser[Boolean] = booleanAttr("mixed")

  lazy val minOccursAttr: Parser[Int] = strAttr("minOccurs") >>= parseInt

  lazy val nameAttr = strAttr("name")

  lazy val nestedParticle: Parser[NestedParticle] = element | groupRef | choice | sequence | any

  lazy val namespaceAttr = optionalAttr(QNameFactory.caching(new LocalName("namespace")))

  lazy val namespaceListAttr: Parser[NsList.Def] = strAttr("namespace") >>= {
    case "##any" => success(NsList.Any)
    case "##other" => success(NsList.Other)
    case s => Parser.traverse(s.split("\\s+").toList)(stringToNsToken).map(NsList.ExDef(_))
  }

  lazy val nillableAttr: Parser[Boolean] = booleanAttr("nillable")

  lazy val noFixedFacet = annotated ~ valueAttr

  lazy val notation: Parser[NotationElem] = xsElem("notation")(annotated ~ nameAttr ~ publicAttr.opt ~ systemAttr.opt) gmap Generic[NotationElem]

  lazy val notNamespaceAttr: Parser[List[NsList.Token]] = strAttr("notNamespace") map (_.split("\\s+").toList) >>= (Parser.traverse(_)(stringToNsToken))

  lazy val notQNameAttr: Parser[List[QnList.Token]] = strAttr("notQName") map (_.split("\\s+").toList) >>= (Parser.traverse(_) {
    case "##defined" => success(QnList.Defined)
    case "##definedSibling" => success(QnList.DefinedSibling)
    case s => resolveQn(s).map(QnList.QnItem(_))
  })

  lazy val notQNameAttrA: Parser[List[QnList.TokenA]] = strAttr("notQName") map (_.split("\\s+").toList) >>= (Parser.traverse(_) {
    case "##defined" => success(QnList.Defined)
    case s => resolveQn(s).map(QnList.QnItem(_))
  })

  lazy val occurs = minOccursAttr.opt ~ maxOccursAttr.opt

  lazy val openAttrs: Parser[OpenAttrsValue] = selectAttrs(_.namespace != XsdNamespace)

  lazy val openContent: Parser[OpenContentElem] = xsElem("openContent")(annotated ~ openContentModeAttr.opt ~ openContentAny.opt) gmap Generic[OpenContentElem]

  lazy val openContentAny: Parser[OpenContentAnyElem] = xsElem("any")(annotated ~ anyAttrGroup) gmap Generic[OpenContentAnyElem]

  lazy val openContentModeAttr: Parser[OpenContentMode] = strAttr("mode") >>= {
    case "none" => success(OpenContentMode.None)
    case "interleave" => success(OpenContentMode.Interleave)
    case "suffix" => success(OpenContentMode.Suffix)
    case s => fail(s"invalid open content mode: $s")
  }

  lazy val overrideElem: Parser[OverrideElem] = xsElem("override")(idAttr ~ schemaLocationAttr ~ either(schemaTopGroupElem, annotation).rep) gmap Generic[OverrideElem]

  lazy val pattern: Parser[PatternElem] = xsElem("pattern")(noFixedFacet) gmap Generic[PatternElem]

  lazy val processContentsAttr: Parser[Pc.Token] = strAttr("processContents") >>= {
    case "skip" => success(Pc.Skip)
    case "lax" => success(Pc.Lax)
    case "strict" => success(Pc.Strict)
    case s => fail(s"invalid process contents value: $s")
  }

  lazy val publicAttr: Parser[String] = strAttr("public")

  lazy val redefinableGroupElem: Parser[RedefinableGroupElem] = simpleType | complexType | groupDef | attributeGroupDef

  lazy val redefine: Parser[RedefineElem] = xsElem("redefine")(idAttr ~ schemaLocationAttr ~ either(redefinableGroupElem, annotation).rep) gmap Generic[RedefineElem]

  lazy val refAttr = qnAttr("ref")

  lazy val schema: Parser[SchemaElem]  = xsElem("schema")(idAttr ~ either(compositionGroupElem, annotation).rep ~ either(schemaTopGroupElem, annotation).rep) gmap Generic[SchemaElem]

  lazy val schemaLocationAttr: Parser[String] = requiredAttr(QNameFactory.caching(new LocalName("schemaLocation")))

  lazy val schemaTopGroupElem: Parser[SchemaTopGroupElem] = redefinableGroupElem | element | attribute | notation

  lazy val selector: Parser[SelectorElem] = xsElem("selector")(annotated ~ xPathAttr ~ xPathDefaultNamespaceAttr.opt) gmap Generic[SelectorElem]

  lazy val sequence: Parser[SequenceElem] = xsElem("sequence")(annotated ~ nestedParticle.rep) gmap Generic[SequenceElem]
  
  lazy val simpleContent: Parser[SimpleContentElem] = xsElem("simpleContent")(annotated ~ (simpleContentRestriction | simpleContentExtension)) gmap Generic[SimpleContentElem]

  lazy val simpleContentExtension: Parser[SimpleContentExtensionElem] = xsElem("extension")(annotated ~ baseAttr ~ attrDecls ~ assert.rep) gmap Generic[SimpleContentExtensionElem]

  lazy val simpleContentRestriction: Parser[SimpleContentRestrictionElem] = xsElem("restriction")(annotated ~ baseAttr ~ simpleRestrictionModel ~ attrDecls ~ assert.rep) gmap Generic[SimpleContentRestrictionElem]

  lazy val simpleDerivationGroupElem: Parser[SimpleDerivationGroupElem] = simpleTypeRestriction | list | union

  lazy val simpleRestrictionModel: Parser[Option[SimpleTypeElem] :: List[FacetElem] :: HNil] = { val r = simpleType.opt ~ facet.rep; r }

  lazy val simpleType: Parser[SimpleTypeElem] = xsElem("simpleType")(annotated ~ nameAttr.opt ~ simpleDerivationGroupElem) gmap Generic[SimpleTypeElem]

  lazy val simpleTypeRestriction: Parser[SimpleTypeRestrictionElem] = xsElem("restriction")(annotated ~ baseAttr.opt ~ simpleRestrictionModel) gmap Generic[SimpleTypeRestrictionElem]

  lazy val sourceAttr: Parser[Option[String]] = optionalAttr(QNameFactory.caching(new LocalName("source")))

  lazy val substitutionGroup: Parser[List[QName]] = qNamesAttr("substitutionGroup")

  lazy val systemAttr: Parser[URI] = anyUriAttr("system")

  lazy val targetNamespaceAttr: Parser[URI] = anyUriAttr("targetNamespace")

  lazy val testAttr: Parser[String] = strAttr("test")

  lazy val typeAttr = qnAttr("type")

  lazy val typeDefParticle: Parser[TypeDefParticle] = groupRef | all | choice | sequence

  lazy val union: Parser[UnionElem] = xsElem("union")(annotated ~ memberTypes.opt ~ simpleType.rep) gmap Generic[UnionElem]

  lazy val unique: Parser[UniqueElem] = xsElem("unique")(keybase) gmap Generic[UniqueElem]

  lazy val useAttr: Parser[Use] = strAttr("use") >>= {
    case "optional" => success(Optional)
    case "required" => success(Required)
    case "prohibited" => success(Prohibited)
    case s => fail(s"invalid use attribute: $s")
  }

  lazy val valueAttr: Parser[String] = requiredAttr(QNameFactory.caching(new LocalName("value")))

  lazy val xPathAttr: Parser[String] = strAttr("xpath")

  lazy val xPathDefaultNamespaceAttr: Parser[NamespaceToken.XPathDefault] = strAttr("xpathDefaultNamespace") >>= {
    case "##defaultNamespace" => success(NamespaceToken.Default)
    case "##targetNamespace" => success(NamespaceToken.Target)
    case "##local" => success(NamespaceToken.Local)
    case s => parseUri.apply(s).map(NamespaceToken.AnyUri(_))
  }

  //
  //
  //

  val dcExtension: PartialFunction[String, Derivation.Extension.type] = { case "extension" => Derivation.Extension }
  val dcRestriction: PartialFunction[String, Derivation.Restriction.type] = { case "restriction" => Derivation.Restriction }
  val dcSubstitution: PartialFunction[String, Derivation.Substitution.type] = { case "substitution" => Derivation.Substitution }

  //
  //
  //
  
  private lazy val openAttrsEndElem: Parser[OpenAttrsValue :: HNil] = { val r = openAttrs ~ endElement; r }

  def either[L, R](l: Parser[L], r: => Parser[R]): Parser[Either[L, R]] = (l map (Left(_))) | (r map (Right(_)))

  private def xsElem[HL <: HList](name: String)(p: => Parser[HL])(implicit ev: Prepend[Location :: HL, OpenAttrsValue :: HNil]) = startElement(QNameFactory.caching(XsdNamespace, new LocalName(name))) ~ p ~ openAttrsEndElem

  private def anyUriAttr(name: String): Parser[URI] = strAttr(name) >>= parseUri

  private val parseInt: String => Parser[Int] = ((s: String) => s.toInt) onEx ((s, ex) => s"invalid integer: $s; exception: ${ex.getMessage}")

  private val parseUri: String => Parser[URI] = ((s: String) => new URI(s)) onEx ((s, ex) => s"invalid uri: $s; exception: ${ex.getMessage}")

  private def strAttr(name: String): Parser[String] = requiredAttr(QNameFactory.caching(new LocalName(name)))

  private def qnAttr(name: String): Parser[QName] = strAttr(name) >>= resolveQn

  private def qNamesAttr(name: String): Parser[List[QName]] = strAttr(name) map (_.split("\\s+").toList) >>= (Parser.traverse(_)(resolveQn))

  private def booleanAttr(name: String): Parser[Boolean] = strAttr(name) >>= {
    case "true" => success(true)
    case "1" => success(true)
    case "false" => success(false)
    case "0" => success(false)
    case s => fail(s"illegal boolean: $s")
  }

  private def derivationCtrlAttr[C <: Derivation.Ctrl](name: String)(pf: PartialFunction[String, C]): Parser[Derivation.CtrlSet[C]] = derivationCtrlStr(strAttr(name))(pf)

  def derivationCtrlStr[C <: Derivation.Ctrl](p: Parser[String])(pf: PartialFunction[String, C]): Parser[Derivation.CtrlSet[C]] = p >>= {
    case "#all" => success(Derivation.All)
    case s => {
      Parser.traverse(s.split("\\s+").toList) {
        case s if pf.isDefinedAt(s) => success(pf(s))
        case s => fail(s"illegal derivation control $s")
      }
    }.map(Derivation.CtrlExSet(_))
  }

  private def stringToNsToken(string: String): Parser[NsList.Token] = string match {
    case "##targetNamespace" => success(NsList.TargetNamespace)
    case "##local" => success(NsList.Local)
    case s => parseUri.apply(s).map(NsList.UriItem(_))
  }

  implicit class FunctionOps[I, O](f: I => O) {
    def onEx(on: (I, Throwable) => String): I => Parser[O] = i => {
      Try(f(i)) match {
        case Success(s) => success(s)
        case Failure(ex) => fail(on(i, ex))
      }
    }
  }

}
