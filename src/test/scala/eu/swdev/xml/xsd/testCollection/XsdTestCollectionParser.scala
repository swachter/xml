package eu.swdev.xml.xsd.testCollection

import java.net.{URI, URL}
import javax.xml.stream.XMLInputFactory
import javax.xml.transform.stream.StreamSource

import eu.swdev.xml.base.Location
import eu.swdev.xml.name._
import eu.swdev.xml.pushparser.{XmlEventReaderInputs, XmlPushParserMod}
import shapeless.ops.hlist.Prepend
import shapeless.{::, Generic, HList, HNil}

object XsdTestCollectionParser extends XmlPushParserMod with XmlEventReaderInputs {

  type Payload = Unit

  val XtNamespace = Namespace("http://www.w3.org/XML/2004/xml-schema-test-suite/")
  val XlNamespace = Namespace("http://www.w3.org/1999/xlink")

  lazy val annotation: Parser[AnnotationElem] = xtElem("annotation")(idAttr ~ either(appInfo, documentation).rep) gmap Generic[AnnotationElem]

  lazy val appInfo: Parser[AppInfoElem] = xtElem("appinfo")(sourceAttr ~ rawXml) gmap Generic[AppInfoElem]

  lazy val documentation: Parser[DocumentationElem] = xtElem("documentation")(sourceAttr ~ langAttr ~ rawXml) gmap Generic[DocumentationElem]

  lazy val expected: Parser[ExpectedElem] = xtElem("expected")(validityAttr ~ pNil) gmap Generic[ExpectedElem]

  lazy val idAttr: Parser[Option[String]] = strAttr("id").opt

  lazy val instanceTest: Parser[InstanceTestElem] = xtElem("instanceTest")(strAttr("name") ~ ref("instanceDocument") ~ expected.opt ~ statusEntry("current").opt ~ statusEntry("prior").rep) gmap Generic[InstanceTestElem]

  lazy val langAttr = optionalAttr(QNameFactory.caching(XmlNamespace, LocalName("lang")))

  lazy val openAttrs: Parser[Map[QName, String]] = selectAttrs(_.namespace != XtNamespace)

  def ref(name: String): Parser[RefElem] = xtElem(name)(strAttr("type").opt ~ strAttr(XlNamespace, "href")) gmap Generic[RefElem]

  lazy val schemaTest: Parser[SchemaTestElem] = xtElem("schemaTest")(strAttr("name") ~ ref("schemaDocument").rep ~ expected.opt ~ statusEntry("current").opt ~ statusEntry("prior").rep) gmap Generic[SchemaTestElem]

  lazy val sourceAttr: Parser[Option[String]] = optionalAttr(QNameFactory.caching(LocalName("source")))

  def statusEntry(name: String): Parser[StatusEntryElem] = xtElem(name)(strAttr("status") ~ strAttr("date") ~ strAttr("bugzilla").opt) gmap Generic[StatusEntryElem]

  lazy val testGroup: Parser[TestGroupElem] = xtElem("testGroup")(annotation.opt ~ strAttr("name") ~ ref("documentationReference").rep ~ schemaTest.opt ~ instanceTest.rep) gmap Generic[TestGroupElem]

  lazy val testResult: Parser[TestResultElem] = xtElem("testResult")(annotation.opt ~ strAttr("validity") ~ strAttr("set") ~ strAttr("group") ~ strAttr("test") ~ strAttr("normalizedLoad").opt) gmap Generic[TestResultElem]

  lazy val testSet: Parser[TestSetElem] = xtElem("testSet")(annotation.opt ~ strAttr("contributor") ~ strAttr("name") ~ testGroup.rep) gmap Generic[TestSetElem]

  lazy val testSuite: Parser[TestSuiteElem] = xtElem("testSuite")(annotation.opt ~ strAttr("name") ~ strAttr("releaseDate") ~ strAttr("schemaVersion") ~ ref("testSetRef").rep) gmap Generic[TestSuiteElem]

  lazy val testSuiteResults: Parser[TestSuiteResultsElem] = xtElem("testSuiteResults")(annotation.opt ~ strAttr("suite") ~ strAttr("processor") ~ strAttr("submitDate") ~ strAttr("publicationPermission").opt ~ testResult.rep) gmap Generic[TestSuiteResultsElem]

  lazy val validityAttr: Parser[ValidityOutcome] = strAttr("validity") >>= {
    case "valid" => success(ValidityOutcome.Valid)
    case "invalid" => success(ValidityOutcome.Invalid)
    case "notKnown" => success(ValidityOutcome.NotKnown)
    case s => fail(s"invalid validity value: $s")
  }

  //

  private lazy val openAttrsEndElem: Parser[Map[QName, String] :: HNil] = { val r = openAttrs ~ endElement; r }


  private def xtElem[HL <: HList](name: String)(p: => Parser[HL])(implicit ev: Prepend[Location :: HL, Map[QName, String] :: HNil]) = startElement(QNameFactory.caching(XtNamespace, LocalName(name))) ~ p ~ openAttrsEndElem

  def either[L, R](l: Parser[L], r: => Parser[R]): Parser[Either[L, R]] = (l map (Left(_))) | (r map (Right(_)))

  private def strAttr(name: String): Parser[String] = requiredAttr(QNameFactory.caching(LocalName(name)))

  private def strAttr(ns: Namespace, name: String): Parser[String] = requiredAttr(QNameFactory.caching(ns, LocalName(name)))


  def inputs(uri: URI): DriveInputs = {
    val reader = XMLInputFactory.newInstance().createXMLEventReader(new StreamSource(uri.toURL.toExternalForm))
    inputs(reader)
  }

  def parseDoc[O](p: Parser[O]): URI => DriveResult[O] = in => document(p).drive(initialState(Some(in), ()), inputs(in))
}

case class AnnotationElem(loc: Location, id: Option[String], seq: Seq[Either[AppInfoElem, DocumentationElem]], openAttrs: Map[QName, String])

case class AppInfoElem(loc: Location, source: Option[String], rawXml: String, openAttrs: Map[QName, String])

case class DocumentationElem(loc: Location, source: Option[String], lang: Option[String], rawXml: String, openAttrs: Map[QName, String])

case class ExpectedElem(loc: Location, validity: ValidityOutcome, openAttrs: Map[QName, String])

case class InstanceTestElem(loc: Location, name: String, instanceDocument: RefElem, expected: Option[ExpectedElem], current: Option[StatusEntryElem], prior: Seq[StatusEntryElem], openAttrs: Map[QName, String])

case class RefElem(loc: Location, tpe: Option[String], href: String, openAttrs: Map[QName, String])

case class SchemaTestElem(loc: Location, name: String, schemaDocuments: Seq[RefElem], expected: Option[ExpectedElem], current: Option[StatusEntryElem], prior: Seq[StatusEntryElem], openAttrs: Map[QName, String])

case class StatusEntryElem(loc: Location, status: String, date: String, bugzilla: Option[String], openAttrs: Map[QName, String])

case class TestGroupElem(loc: Location, annotation: Option[AnnotationElem], name: String, docRefs: Seq[RefElem], schemaTest: Option[SchemaTestElem], instanceTests: Seq[InstanceTestElem], openAttrs: Map[QName, String])

case class TestResultElem(loc: Location, annotation: Option[AnnotationElem], validity: String, set: String, group: String, test: String, normalizedLoad: Option[String], openAttrs: Map[QName, String])

case class TestSetElem(loc: Location, annotation: Option[AnnotationElem], contributor: String, name: String, testGroups: Seq[TestGroupElem], openAttrs: Map[QName, String])

case class TestSuiteElem(loc: Location, annotation: Option[AnnotationElem], name: String, releaseDate: String, schemaVersion: String, testSetRefs: Seq[RefElem], openAttrs: Map[QName, String])

case class TestSuiteResultsElem(loc: Location, annotation: Option[AnnotationElem], suite: String, processor: String, submitDate: String, publicationPermission: Option[String], testResults: Seq[TestResultElem], openAttrs: Map[QName, String])

trait ValidityOutcome

object ValidityOutcome {
  object Valid extends ValidityOutcome
  object Invalid extends ValidityOutcome
  object NotKnown extends ValidityOutcome
}