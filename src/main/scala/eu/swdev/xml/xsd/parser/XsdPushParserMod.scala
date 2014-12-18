package eu.swdev.xml.xsd.parser

import java.net.URI

import eu.swdev.xml.base._
import eu.swdev.xml.log.Messages
import eu.swdev.xml.name._
import eu.swdev.xml.pushparser.XmlPushParserMod
import eu.swdev.xml.schema.OpenContentMode.Interleave
import eu.swdev.xml.schema._
import eu.swdev.xml.xsd.cmp._
import shapeless.ops.hlist.Prepend
import shapeless.{::, Generic, HList, HNil}

import scala.util.{Failure, Success, Try}

trait XsdPushParserMod extends XmlPushParserMod {

  def parseSchemaDocument(inputs: DriveInputs): (Messages, Option[SchemaElem]) = {
    val driveResult = parseDocument(schema, initialState, inputs)
    (driveResult._2.log, driveResult._1)
  }

  type Payload = Int

  def initialState: State = initialState(0)

  type OpenAttrsValue = Map[QName, String]
  type IdAttrValue = Option[String]

  lazy val abstractAttr = booleanAttr("abstract")

  lazy val all: Parser[AllElem] = xsElem("all")(annotated ~ occurs ~ nestedParticle.rep) gmap Generic[AllElem]

  lazy val alternative: Parser[AlternativeElem] = xsElem("alternative")(annotated ~ testAttr.opt ~ typeAttr.opt ~ xPathDefaultNamespaceAttr.opt ~ either(simpleType, complexType).opt) gmap Generic[AlternativeElem]

  lazy val annotated: Parser[IdAttrValue :: Option[AnnotationElem] :: HNil] = { val r = idAttr ~ annotation.opt; r }

  lazy val annotation: Parser[AnnotationElem] = xsElem("annotation")(idAttr ~ either(appInfo, documentation).rep) gmap Generic[AnnotationElem]

  lazy val any: Parser[AnyElem] = xsElem("any")(annotated ~ anyAttrGroup ~ notQNameAttr.opt ~ occurs) gmap Generic[AnyElem]

  lazy val anyAttrGroup = namespaceListAttr.opt ~ notNamespaceAttr.opt ~ processContentsAttr.some(ProcessContents.Strict)

  lazy val anyAttribute: Parser[AnyAttributeElem] = xsElem("anyAttribute")(annotated ~ anyAttrGroup ~ notQNameAttrA.opt) gmap Generic[AnyAttributeElem]

  lazy val appInfo: Parser[AppInfoElem] = xsElem("appinfo")(sourceAttr ~ rawXml) gmap Generic[AppInfoElem]

  lazy val appliesToEmptyAttr: Parser[Boolean] = booleanAttr("appliesToEmpty")

  lazy val assert: Parser[AssertElem] = xsElem("assert")(annotated ~ testAttr ~ xPathDefaultNamespaceAttr.opt ~ namespaces) gmap Generic[AssertElem]

  lazy val attrDecls = either(attributeL, attributeGroupRef).rep ~ anyAttribute.opt

  lazy val attributeFormDefaultAttr: Parser[SomeValue[Form]] = formAttrDef("attributeFormDefault").some(Form.Unqualified)

  lazy val attributeG: Parser[AttributeElemG] = xsElem("attribute")(annotated ~ nameAttr.map(Some(_)) ~ typeAttr.opt ~ defaultAttr.opt ~ fixedAttr.opt ~ inheritableAttr.opt ~ simpleType.opt) gmap Generic[AttributeElemG]
  lazy val attributeL: Parser[AttributeElemL] = xsElem("attribute")(annotated ~ defRef ~ typeAttr.opt ~ useAttr.opt ~ defaultAttr.opt ~ fixedAttr.opt ~ formAttr.opt ~ targetNamespaceAttr.opt ~ inheritableAttr.opt ~ simpleType.opt) gmap Generic[AttributeElemL]

  lazy val attributeGroupDef: Parser[AttributeGroupDefElem] = xsElem("attributeGroup")(annotated ~ nameAttr ~ attrDecls) gmap Generic[AttributeGroupDefElem]

  lazy val attributeGroupRef: Parser[AttributeGroupRefElem] = xsElem("attributeGroup")(annotated ~ refAttr) gmap Generic[AttributeGroupRefElem]

  lazy val baseAttr: Parser[QName] = qnAttr("base")

  lazy val blockDefaultAttr: Parser[SomeValue[RelationSet[BlockCtrl]]] = derivationCtrlAttr("blockDefault")(relExtension orElse relRestriction orElse relSubstitution).some(RelationSet.Items(Nil))

  lazy val choice: Parser[ChoiceElem] = xsElem("choice")(annotated ~ occurs ~ nestedParticle.rep) gmap Generic[ChoiceElem]

  lazy val complexContent: Parser[ComplexContentElem] = xsElem("complexContent")(annotated ~ mixedAttr.opt ~ (complexRestriction | complexExtension)) gmap Generic[ComplexContentElem]
  
  lazy val complexContentAbbrev: Parser[ComplexContentAbbrev] = getState.map(_.lastLocation.get) ~ complexContentModel gmap Generic[ComplexContentAbbrev]

  lazy val complexContentModel = openContent.opt ~ typeDefParticle.opt ~ attrDecls ~ assert.rep

  lazy val complexExtension: Parser[ComplexExtensionElem] = xsElem("extension")(annotated ~ baseAttr ~ complexContentModel) gmap Generic[ComplexExtensionElem]

  lazy val complexRestriction: Parser[ComplexRestrictionElem] = xsElem("restriction")(annotated ~ baseAttr ~ complexContentModel) gmap Generic[ComplexRestrictionElem]

  lazy val complexType: Parser[ComplexTypeElem] = xsElem("complexType")(annotated ~ someNameAttr(nameAttr) ~ mixedAttr.opt ~ abstractAttr.some(false) ~ complexTypeFinalAttr.opt ~ complexTypeBlockAttr.opt ~ defaultAttributesApplyAttr.opt ~ complexTypeContent) gmap Generic[ComplexTypeElem]

  lazy val complexTypeBlockAttr: Parser[RelationSet[CtBlockCtrl]] = derivationCtrlAttr("block")(relExtension orElse relRestriction)

  lazy val complexTypeFinalAttr: Parser[RelationSet[CtDerivationCtrl]] = derivationCtrlAttr("final")(relExtension orElse relRestriction)

  lazy val complexTypeContent: Parser[ComplexTypeContentCmp] = simpleContent | complexContent | complexContentAbbrev

  lazy val compositionGroupElem: Parser[CompositionGroupElem] = include | importElem | redefine | overrideElem

  lazy val defaultAttr: Parser[String] = strAttr("default")

  lazy val defaultAttributesAttr: Parser[QName] = qnAttr("defaultAttributes")

  lazy val defaultOpenContent: Parser[DefaultOpenContentElem] = xsElem("defaultOpenContent")(annotated ~ appliesToEmptyAttr.some(false) ~ defaultOpenContentModeAttr.some(Interleave) ~ openContentAny) gmap Generic[DefaultOpenContentElem]

  lazy val defaultOpenContentModeAttr: Parser[DefaultOpenContentMode] = strAttr("mode") >>= {
    case "interleave" => success(OpenContentMode.Interleave)
    case "suffix" => success(OpenContentMode.Suffix)
    case s => fail(s"invalid open content mode: $s")
  }

  lazy val defaultAttributesApplyAttr: Parser[Boolean] = booleanAttr("defaultAttributesApply")

  lazy val defRef = nameAttr.opt ~ refAttr.opt

  lazy val documentation: Parser[DocumentationElem] = xsElem("documentation")(sourceAttr ~ langAttr ~ rawXml) gmap Generic[DocumentationElem]

  lazy val element: Parser[ElementElem] = xsElem("element")(annotated ~ defRef ~ typeAttr.opt ~ substitutionGroup.opt ~ occurs ~ defaultAttr.opt ~ fixedAttr.opt ~ nillableAttr.opt ~ abstractAttr.opt ~ elementFinalAttr.opt ~ elementBlockAttr.opt ~ formAttr.opt ~ targetNamespaceAttr.opt ~ either(simpleType, complexType).opt ~ alternative.rep ~ identityConstraint.rep) gmap Generic[ElementElem ]

  lazy val elementBlockAttr: Parser[RelationSet[ElemBlockCtrl]] = derivationCtrlAttr("block")(relExtension orElse relRestriction orElse relSubstitution)

  lazy val elementFinalAttr: Parser[RelationSet[ElemFinalCtrl]] = derivationCtrlAttr("final")(relExtension orElse relRestriction)

  lazy val elementFormDefaultAttr: Parser[SomeValue[Form]] = formAttrDef("elementFormDefault").some(Form.Unqualified)

  lazy val facet: Parser[FacetElem] = assertion | enumeration | explicitTimezone | fractionDigits | length | maxExclusive | maxInclusive | maxLength | minExclusive | minInclusive | minLength | pattern | totalDigits | whitespace

  lazy val field: Parser[FieldElem] = xsElem("field")(annotated ~ xPathAttr ~ xPathDefaultNamespaceAttr.opt) gmap Generic[FieldElem]

  def finalAttrDef(name: String): Parser[SomeValue[RelationSet[TypeDerivationCtrl]]] = derivationCtrlAttr(name)(relExtension orElse relRestriction orElse relList orElse relUnion).some(RelationSet.Items(Nil))
  lazy val finalDefaultAttr = finalAttrDef("finalDefault")
  lazy val finalAttr = finalAttrDef("final")

  lazy val fixedAttr = strAttr("fixed")

  lazy val formAttr = formAttrDef("formAttr")

  def formAttrDef(name: String): Parser[Form] = strAttr(name) >>= {
    case "qualified" => success(Form.Qualified)
    case "unqualified" => success(Form.Unqualified)
    case s => fail(s"invalid form: $s")
  }

  lazy val groupDef: Parser[GroupDefElem] = xsElem("group")(annotated ~ nameAttr ~ (all | choice | sequence)) gmap Generic[GroupDefElem]

  lazy val groupRef: Parser[GroupRefElem] = xsElem("group")(annotated ~ refAttr ~ occurs) gmap Generic[GroupRefElem]

  lazy val idAttr: Parser[IdAttrValue] = strAttr("id").opt

  lazy val identityConstraint: Parser[IdentityConstraintGroupElem] = unique | key | keyref

  lazy val importElem: Parser[ImportElem] = xsElem("import")(annotated ~ schemaLocationAttr.opt ~ namespaceAttr) gmap Generic[ImportElem]

  lazy val include: Parser[IncludeElem] = xsElem("include")(annotated ~ schemaLocationAttr) gmap Generic[IncludeElem]

  lazy val inheritableAttr: Parser[Boolean] = booleanAttr("inheritable")

  lazy val itemTypeAttr: Parser[QName] = qnAttr("itemType")

  lazy val key: Parser[KeyElem] = xsElem("key")(keybase) gmap Generic[KeyElem]

  lazy val keybase = annotated ~ strAttr("name").opt ~ qnAttr("ref").opt ~ (selector & field.rep1).opt

  lazy val keyref: Parser[KeyRefElem] = xsElem("keyref")(keybase ~ qnAttr("refer").opt) gmap Generic[KeyRefElem]

  lazy val langAttr = optionalAttr(QNameFactory.caching(XmlNamespace, LocalName("lang")))

  lazy val list: Parser[ListElem] = xsElem("list")(annotated ~ itemTypeAttr.opt ~ simpleType.opt) gmap Generic[ListElem]

  lazy val maxOccursAttr: Parser[MaxOccurs] = strAttr("maxOccurs") >>= {
    case "unbounded" => success(MaxOccurs.Unbounded)
    case s => parseInt.apply(s).map(MaxOccurs.Bounded(_))
  }

  lazy val memberTypes: Parser[List[QName]] = qNamesAttr("memberTypes")

  lazy val mixedAttr: Parser[Boolean] = booleanAttr("mixed")

  lazy val minOccursAttr: Parser[Int] = strAttr("minOccurs") >>= parseInt

  lazy val nameAttr = strAttr("name")

  lazy val nestedParticle: Parser[NestedParticleCmp] = element | groupRef | choice | sequence | any

  lazy val namespaceAttr = optionalAttr(QNameFactory.caching(LocalName("namespace")))

  lazy val namespaceListAttr: Parser[NamespaceDefToken] = strAttr("namespace") >>= {
    case "##any" => success(NamespaceDefToken.Any)
    case "##other" => success(NamespaceDefToken.Other)
    case s => Parser.traverse(s.split("\\s+").toList)(stringToNsToken).map(NamespaceDefToken.Items(_))
  }

  lazy val nillableAttr: Parser[Boolean] = booleanAttr("nillable")

  lazy val notation: Parser[NotationElem] = xsElem("notation")(annotated ~ nameAttr ~ publicAttr.opt ~ systemAttr.opt) gmap Generic[NotationElem]

  lazy val notNamespaceAttr: Parser[List[NamespaceItemToken]] = strAttr("notNamespace") map (_.split("\\s+").toList) >>= (Parser.traverse(_)(stringToNsToken))

  lazy val notQNameAttr: Parser[List[QNameItem]] = strAttr("notQName") map (_.split("\\s+").toList) >>= (Parser.traverse(_) {
    case "##defined" => success(QNameItem.Defined)
    case "##definedSibling" => success(QNameItem.DefinedSibling)
    case s => resolveQn(s).map(QNameItem.Qn(_))
  })

  lazy val notQNameAttrA: Parser[List[QNameItemA]] = strAttr("notQName") map (_.split("\\s+").toList) >>= (Parser.traverse(_) {
    case "##defined" => success(QNameItem.Defined)
    case s => resolveQn(s).map(QNameItem.Qn(_))
  })

  lazy val occurs = minOccursAttr.opt ~ maxOccursAttr.opt

  lazy val openAttrs: Parser[OpenAttrsValue] = selectAttrs(_.namespace != XsdNamespace)

  lazy val openContent: Parser[OpenContentElem] = xsElem("openContent")(annotated ~ openContentModeAttr.some(OpenContentMode.Interleave) ~ openContentAny.opt) gmap Generic[OpenContentElem]

  lazy val openContentAny: Parser[OpenContentAnyElem] = xsElem("any")(annotated ~ anyAttrGroup) gmap Generic[OpenContentAnyElem]

  lazy val openContentModeAttr: Parser[OpenContentMode] = strAttr("mode") >>= {
    case "none" => success(OpenContentMode.None)
    case "interleave" => success(OpenContentMode.Interleave)
    case "suffix" => success(OpenContentMode.Suffix)
    case s => fail(s"invalid open content mode: $s")
  }

  lazy val overrideElem: Parser[OverrideElem] = xsElem("override")(idAttr ~ schemaLocationAttr ~ either(schemaTopGroupElem, annotation).rep) gmap Generic[OverrideElem]

  lazy val processContentsAttr: Parser[ProcessContents] = strAttr("processContents") >>= {
    case "skip" => success(ProcessContents.Skip)
    case "lax" => success(ProcessContents.Lax)
    case "strict" => success(ProcessContents.Strict)
    case s => fail(s"invalid process contents value: $s")
  }

  lazy val publicAttr: Parser[String] = strAttr("public")

  lazy val redefinableGroupElem: Parser[RedefinableGroupElem] = simpleType | complexType | groupDef | attributeGroupDef

  lazy val redefine: Parser[RedefineElem] = xsElem("redefine")(idAttr ~ schemaLocationAttr ~ either(redefinableGroupElem, annotation).rep) gmap Generic[RedefineElem]

  lazy val refAttr = qnAttr("ref")

  lazy val schema: Parser[SchemaElem]  = xsElem("schema")(idAttr ~ targetNamespaceAttr.opt ~ versionAttr.opt ~ finalDefaultAttr ~ blockDefaultAttr ~ attributeFormDefaultAttr ~ elementFormDefaultAttr ~ defaultAttributesAttr.opt ~ xPathDefaultNamespaceAttr.some(XPathDefaultNamespace.Local) ~ langAttr ~ either(compositionGroupElem, annotation).rep ~ defaultOpenContent.opt ~ either(schemaTopGroupElem, annotation).rep) gmap Generic[SchemaElem]

  lazy val schemaLocationAttr: Parser[String] = requiredAttr(QNameFactory.caching(LocalName("schemaLocation")))

  lazy val schemaTopGroupElem: Parser[SchemaTopGroupElem] = redefinableGroupElem | element | attributeG | notation

  lazy val selector: Parser[SelectorElem] = xsElem("selector")(annotated ~ xPathAttr ~ xPathDefaultNamespaceAttr.opt) gmap Generic[SelectorElem]

  lazy val sequence: Parser[SequenceElem] = xsElem("sequence")(annotated ~ occurs ~nestedParticle.rep) gmap Generic[SequenceElem]
  
  lazy val simpleContent: Parser[SimpleContentElem] = xsElem("simpleContent")(annotated ~ (simpleContentRestriction | simpleContentExtension)) gmap Generic[SimpleContentElem]

  lazy val simpleContentExtension: Parser[SimpleContentExtensionElem] = xsElem("extension")(annotated ~ baseAttr ~ attrDecls ~ assert.rep) gmap Generic[SimpleContentExtensionElem]

  lazy val simpleContentRestriction: Parser[SimpleContentRestrictionElem] = xsElem("restriction")(annotated ~ baseAttr ~ simpleRestrictionModel ~ attrDecls ~ assert.rep ~ syntheticTypeName) gmap Generic[SimpleContentRestrictionElem]

  lazy val simpleDerivationGroupElem: Parser[SimpleDerivationGroupElem] = simpleTypeRestriction | list | union

  lazy val simpleRestrictionModel: Parser[Option[SimpleTypeElem] :: List[FacetElem] :: HNil] = { val r = simpleType.opt ~ facet.rep; r }

  lazy val simpleType: Parser[SimpleTypeElem] = xsElem("simpleType")(annotated ~ someNameAttr(nameAttr) ~ finalAttr ~ simpleDerivationGroupElem) gmap Generic[SimpleTypeElem]

  lazy val simpleTypeRestriction: Parser[SimpleTypeRestrictionElem] = xsElem("restriction")(annotated ~ baseAttr.opt ~ simpleRestrictionModel) gmap Generic[SimpleTypeRestrictionElem]

  lazy val sourceAttr: Parser[Option[String]] = optionalAttr(QNameFactory.caching(LocalName("source")))

  lazy val substitutionGroup: Parser[List[QName]] = qNamesAttr("substitutionGroup")

  lazy val systemAttr: Parser[URI] = anyUriAttr("system")

  lazy val targetNamespaceAttr: Parser[URI] = anyUriAttr("targetNamespace")

  lazy val testAttr: Parser[String] = strAttr("test")

  lazy val typeAttr = qnAttr("type")

  lazy val typeDefParticle: Parser[TypeDefParticleCmp] = groupRef | all | choice | sequence

  lazy val union: Parser[UnionElem] = xsElem("union")(annotated ~ memberTypes.opt ~ simpleType.rep) gmap Generic[UnionElem]

  lazy val unique: Parser[UniqueElem] = xsElem("unique")(keybase) gmap Generic[UniqueElem]

  lazy val useAttr: Parser[Use] = strAttr("use") >>= {
    case "optional" => success(Use.Optional)
    case "required" => success(Use.Required)
    case "prohibited" => success(Use.Prohibited)
    case s => fail(s"invalid use attribute: $s")
  }

  lazy val valueAttr: Parser[String] = requiredAttr(QNameFactory.caching(LocalName("value")))

  lazy val versionAttr: Parser[String] = strAttr("version")

  lazy val xPathAttr: Parser[String] = strAttr("xpath")

  lazy val xPathDefaultNamespaceAttr: Parser[XPathDefaultNamespace] = strAttr("xpathDefaultNamespace") >>= {
    case "##defaultNamespace" => success(XPathDefaultNamespace.Default)
    case "##targetNamespace" => success(XPathDefaultNamespace.Target)
    case "##local" => success(XPathDefaultNamespace.Local)
    case s => parseUri.apply(s).map(XPathDefaultNamespace.Uri(_))
  }

  //
  // facets
  //

  def noFixedFacet(name: String) = xsElem(name)(annotated ~ valueAttr)

  def facet(name: String) = xsElem(name)(annotated ~ valueAttr ~ booleanAttr("fixed").opt)

  def facet[X](name: String, bind: String => Parser[X]) = xsElem(name)(annotated ~ (valueAttr >>= bind) ~ booleanAttr("fixed").opt)

  lazy val assertion: Parser[AssertElem] = xsElem("assertion")(annotated ~ testAttr ~ xPathDefaultNamespaceAttr.opt ~ namespaces) gmap Generic[AssertElem]

  lazy val explicitTimezone: Parser[ExplicitTimeZoneElem] = facet("explicitTimezone", (s: String) => s match {
    case "optional" => success(ExplicitTimeZone.Optional)
    case "required" => success(ExplicitTimeZone.Required)
    case "prohibited" => success(ExplicitTimeZone.Prohibited)
    case s => fail(s"invalid explicit timezone value: $s")
  }) gmap Generic[ExplicitTimeZoneElem]

  lazy val enumeration: Parser[EnumerationElem] = noFixedFacet("enumeration") ~ namespaces gmap Generic[EnumerationElem]

  lazy val fractionDigits: Parser[FractionDigitsElem] = facet("fractionDigits", parseInt) gmap Generic[FractionDigitsElem]

  lazy val length: Parser[LengthElem] = facet("length", parseInt) gmap Generic[LengthElem]

  lazy val maxExclusive: Parser[MaxExclusiveElem] = facet("maxExclusive") ~ namespaces gmap Generic[MaxExclusiveElem]

  lazy val maxInclusive: Parser[MaxInclusiveElem] = facet("maxInclusive") ~ namespaces gmap Generic[MaxInclusiveElem]

  lazy val maxLength: Parser[MaxLengthElem] = facet("maxLength", parseInt) gmap Generic[MaxLengthElem]

  lazy val minExclusive: Parser[MinExclusiveElem] = facet("minExclusive") ~ namespaces gmap Generic[MinExclusiveElem]

  lazy val minInclusive: Parser[MinInclusiveElem] = facet("minInclusive") ~ namespaces gmap Generic[MinInclusiveElem]

  lazy val minLength: Parser[MinLengthElem] = facet("minLength", parseInt) gmap Generic[MinLengthElem]

  lazy val pattern: Parser[PatternElem] = noFixedFacet("pattern") gmap Generic[PatternElem]

  lazy val totalDigits: Parser[TotalDigitsElem] = facet("totalDigits", parseInt) gmap Generic[TotalDigitsElem]

  lazy val whitespace: Parser[WhitespaceElem] = facet("whiteSpace", (s: String) => s match {
    case "preserve" => success(WhitespaceProcessing.Preserve)
    case "replace" => success(WhitespaceProcessing.Replace)
    case "collapse" => success(WhitespaceProcessing.Preserve)
    case s => fail(s"invalid whitepsace facet: $s")
  }) gmap Generic[WhitespaceElem]

  //
  //
  //

  val relExtension: PartialFunction[String, Relation.Extension.type] = { case "extension" => Relation.Extension }
  val relRestriction: PartialFunction[String, Relation.Restriction.type] = { case "restriction" => Relation.Restriction }
  val relSubstitution: PartialFunction[String, Relation.Substitution.type] = { case "substitution" => Relation.Substitution }
  val relList: PartialFunction[String, Relation.List.type] = { case "list" => Relation.List }
  val relUnion: PartialFunction[String, Relation.Union.type] = { case "union" => Relation.Union }

  //
  //
  //
  
  private lazy val openAttrsEndElem: Parser[OpenAttrsValue :: HNil] = { val r = openAttrs ~ endElement; r }

  def either[L, R](l: Parser[L], r: => Parser[R]): Parser[Either[L, R]] = (l map (Left(_))) | (r map (Right(_)))

  private def xsElem[HL <: HList](name: String)(p: => Parser[HL])(implicit ev: Prepend[Location :: HL, OpenAttrsValue :: HNil]) = startElement(QNameFactory.caching(XsdNamespace, LocalName(name))) ~ p ~ openAttrsEndElem

  private def anyUriAttr(name: String): Parser[URI] = strAttr(name) >>= parseUri

  private val parseInt: String => Parser[Int] = ((s: String) => s.toInt) onEx ((s, ex) => s"invalid integer: $s; exception: ${ex.getMessage}")

  private val parseUri: String => Parser[URI] = ((s: String) => new URI(s)) onEx ((s, ex) => s"invalid uri: $s; exception: ${ex.getMessage}")

  private def strAttr(name: String): Parser[String] = requiredAttr(QNameFactory.caching(LocalName(name)))

  private def qnAttr(name: String): Parser[QName] = strAttr(name) >>= resolveQn

  private def qNamesAttr(name: String): Parser[List[QName]] = strAttr(name) map (_.split("\\s+").toList) >>= (Parser.traverse(_)(resolveQn))

  val namespaces: Parser[Namespaces] = getState.map(_.namespacesStack.head)

  private def booleanAttr(name: String): Parser[Boolean] = strAttr(name) >>= {
    case "true" => success(true)
    case "1" => success(true)
    case "false" => success(false)
    case "0" => success(false)
    case s => fail(s"illegal boolean: $s")
  }

  private def derivationCtrlAttr[R <: Relation](name: String)(pf: PartialFunction[String, R]): Parser[RelationSet[R]] = derivationCtrlStr(strAttr(name))(pf)

  def derivationCtrlStr[R <: Relation](p: Parser[String])(pf: PartialFunction[String, R]): Parser[RelationSet[R]] = p >>= {
    case "#all" => success(RelationSet.All)
    case s => {
      Parser.traverse(s.split("\\s+").toList) {
        case s if pf.isDefinedAt(s) => success(pf(s))
        case s => fail(s"illegal derivation control $s")
      }
    }.map(RelationSet.Items(_))
  }

  private def someNameAttr(p: Parser[String]): Parser[SomeValue[String]] = for {
    o <- p.opt
    p <- getPayload
    _ <- setPayload(p + (if (o.isDefined) 0 else 1))
  } yield {
    o.fold[SomeValue[String]](DefaultValue(s"##type$p##"))(DefinedValue(_))
  }

  private def syntheticTypeName: Parser[String] = for {
    p <- getPayload
    _ <- setPayload(p + 1)
  } yield s"##type$p##"

  private def stringToNsToken(string: String): Parser[NamespaceItemToken] = string match {
    case "##targetNamespace" => success(NamespaceItemToken.TargetNamespace)
    case "##local" => success(NamespaceItemToken.Local)
    case s => parseUri.apply(s).map(NamespaceItemToken.Uri(_))
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
