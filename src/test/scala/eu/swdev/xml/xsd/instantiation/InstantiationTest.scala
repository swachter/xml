package eu.swdev.xml.xsd.instantiation

import java.io.StringReader
import java.net.URI
import javax.xml.transform.stream.StreamSource

import eu.swdev.xml.log._
import eu.swdev.xml.name.{LocalName, Namespace}
import eu.swdev.xml.pushparser.XmlEventReaderInputs
import eu.swdev.xml.schema.{Type, Schema}
import eu.swdev.xml.xsd.parser.XsdPushParserMod
import org.scalatest.{Inside, FunSuite}

/**
  */
class InstantiationTest extends FunSuite with Inside {

  object sut extends SchemaInstantiator with SchemaParser with SimpleSchemaStore with StdResolveImport with SchemaLoader {

    override def builtInSchemas: Iterable[Schema] = Seq(Schema.builtInSchema)

    override val xsdParsers = new XsdPushParserMod with XmlEventReaderInputs {}

    override def resolveInclude(schemaLocation: String, baseUri: Option[URI]): (Messages, Option[(xsdParsers.DriveInputs, Option[URI])]) = {
      (prepend(s"can not resolve schema for schema location: $schemaLocation", emptyMessages), None)
    }

    def parse(string: String): (Messages, Option[Schema]) = {
      val source = new StreamSource(new StringReader(string))
      val inputs = xsdParsers.inputs(source)
      loadSchema(inputs, None)
    }

  }

  test("simple types") {

    inside(sut.parse(
      """
        |<schema
        |  targetNamespace="http://www.companio.de/f5/config"
        |  xmlns="http://www.w3.org/2001/XMLSchema"
        |  xmlns:config="http://www.companio.de/f5/config"
        |  xmlns:jb="http://www.jbind.org"
        |  elementFormDefault="qualified">
        |</schema>
      """.stripMargin)) {
      case (_, Some(_)) =>
    }

    inside(sut.parse(
      """
        |<schema
        |  targetNamespace="http://www.companio.de/f5/config"
        |  xmlns="http://www.w3.org/2001/XMLSchema"
        |  xmlns:config="http://www.companio.de/f5/config"
        |  xmlns:jb="http://www.jbind.org"
        |  elementFormDefault="qualified">
        |  <simpleType name="simpleType">
        |    <restriction base="int"/>
        |  </simpleType>
        |  <simpleType name="list">
        |    <list itemType="long"/>
        |  </simpleType>
        |  <simpleType name="union">
        |   <union memberTypes="int string"/>
        |  </simpleType>
        |</schema>
      """.stripMargin)) {
      case (_, Some(schema)) => {
        assert(schema.symbolTable.get[Type](LocalName("simpleType")).isDefined)
        assert(schema.symbolTable.get[Type](LocalName("list")).isDefined)
        assert(schema.symbolTable.get[Type](LocalName("union")).isDefined)
      }
    }
  }

  test("msData/wildcards/wildZ008.xsd") {
    inside(sut.parse(
      """
<xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
 <xs:complexType name="A">
  <xs:sequence>
   <xs:any processContents="strict" namespace="##any" />
  </xs:sequence>
 </xs:complexType>

 <xs:complexType name="B">
  <xs:complexContent>
  <xs:restriction base="A">
   <xs:sequence>
    <xs:any processContents="skip" namespace="http://www.example.com" />
   </xs:sequence>
  </xs:restriction>
  </xs:complexContent>
 </xs:complexType>
</xs:schema>
      """)) {
      case (h :: _, Some(_)) =>
    }

  }

  test("msData/particles/particlesIe002.xsd") {
    inside(sut.parse(
      """
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema" targetNamespace="http://xsdtesting" xmlns:x="http://xsdtesting" elementFormDefault="qualified">
        <xsd:complexType name="base">
                <xsd:choice>
                        <xsd:element name="e1" minOccurs="0" maxOccurs="unbounded"/>
                        <xsd:element name="e2" minOccurs="0" maxOccurs="unbounded"/>
                </xsd:choice>
        </xsd:complexType>
        <xsd:complexType name="testing">
                <xsd:complexContent>
                        <xsd:restriction base="x:base">
                                <xsd:choice>
                                        <xsd:element name="e1" minOccurs="0" maxOccurs="1"/>
                                        <xsd:element name="e2" minOccurs="0" maxOccurs="1"/>
                                </xsd:choice>
                        </xsd:restriction>
                </xsd:complexContent>
        </xsd:complexType>
        <xsd:element name="doc" type="x:testing"/>
</xsd:schema>
      """)) {
      case (Nil, Some(_)) =>
    }

  }

}
