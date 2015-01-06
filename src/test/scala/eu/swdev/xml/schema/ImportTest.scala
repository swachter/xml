package eu.swdev.xml.schema

import java.io.StringReader
import java.net.{URI, URL}
import javax.xml.transform.stream.StreamSource

import eu.swdev.xml.log._
import eu.swdev.xml.name.Namespace
import eu.swdev.xml.pushparser.XmlEventReaderInputs
import eu.swdev.xml.xsd.instantiation.JobMod.SchemaImportHint
import eu.swdev.xml.xsd.instantiation._
import eu.swdev.xml.xsd.parser.XsdPushParserMod
import org.scalatest.{Inside, FunSuite}

/**
  */
class ImportTest extends FunSuite with Inside {

  // boeingData/ipo5/ipo.xsd
  val schema = """
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema"
 xmlns:ipo="http://www.example.com/IPO" targetNamespace="http://www.example.com/IPO"
 xmlns:add="http://www.example.com/add"
 elementFormDefault="qualified"
>
        <xsd:import namespace="http://www.example.com/add" schemaLocation="address.xsd"/>

        <xsd:include schemaLocation="itematt.xsd"/>

        <xsd:element name="purchaseOrder" type="ipo:PurchaseOrderType"/>

        <xsd:element name="comment" type="xsd:string" abstract="true"/>

        <xsd:element name="shipComment" type="xsd:normalizedString" substitutionGroup="ipo:comment"/>

        <xsd:element name="customerComment" type="xsd:normalizedString" substitutionGroup="ipo:comment"/>

        <xsd:complexType name="PurchaseOrderType">
                <xsd:sequence>
                        <xsd:choice>
                                <xsd:group ref="ipo:shipAndBill"/>
                                <xsd:element name="singleAddress" type="add:AddressType"/>
                        </xsd:choice>
                        <xsd:element ref="ipo:comment" minOccurs="0"/>
                        <xsd:element name="items" type="ipo:ItemsType"/>
                </xsd:sequence>
                <xsd:attribute name="orderDate" type="xsd:date"/>
        </xsd:complexType>

        <xsd:group name="shipAndBill">
                <xsd:sequence>
                        <xsd:element name="shipTo" type="add:AddressType"/>
                        <xsd:element name="billTo" type="add:AddressType"/>
                </xsd:sequence>
        </xsd:group>

        <xsd:complexType name="ItemsType" mixed="true">
                <xsd:sequence>
                        <xsd:element name="item" minOccurs="0" maxOccurs="unbounded">
                                <xsd:complexType>
                                        <xsd:sequence>
                                                <xsd:element name="productName" type="xsd:string"/>
                                                <xsd:element name="quantity">
                                                        <xsd:simpleType>
                                                                <xsd:restriction base="xsd:positiveInteger">
                                                                        <xsd:maxExclusive value="100"/>
                                                                </xsd:restriction>
                                                        </xsd:simpleType>
                                                </xsd:element>
                                                <xsd:element name="USPrice" type="xsd:decimal"/>
                                                <xsd:element ref="ipo:comment" minOccurs="0" maxOccurs="2"/>
                                                <xsd:element name="shipDate" type="xsd:date" minOccurs="0"/>
                                        </xsd:sequence>
                                        <xsd:attributeGroup ref="ipo:ItemDelivery"/>
                                        <!-- attributeGroup replaces individual declarations -->
                                </xsd:complexType>
                        </xsd:element>
                </xsd:sequence>
        </xsd:complexType>

        <xsd:complexType name="USAddress">
                <xsd:complexContent>
                        <xsd:extension base="add:AddressType">
                                <xsd:sequence>
                                        <xsd:element name="state" type="ipo:USState"/>
                                        <xsd:element name="zip" type="xsd:positiveInteger"/>
                                </xsd:sequence>
                        </xsd:extension>
                </xsd:complexContent>
        </xsd:complexType>

        <xsd:complexType name="UKAddress">
                <xsd:complexContent>
                        <xsd:extension base="add:AddressType">
                                <xsd:sequence>
                                        <xsd:element name="postcode" type="ipo:UKPostcode"/>
                                </xsd:sequence>
                                <xsd:attribute name="exportCode" type="xsd:positiveInteger" fixed="1"/>
                        </xsd:extension>
                </xsd:complexContent>
        </xsd:complexType>

        <xsd:simpleType name="USState">
                <xsd:restriction base="xsd:string">
                        <xsd:enumeration value="AK"/>
                        <xsd:enumeration value="AL"/>
                        <xsd:enumeration value="AR"/>
                        <xsd:enumeration value="CA"/>
                        <xsd:enumeration value="PA"/>
                        <!-- and so on ... -->
                </xsd:restriction>
        </xsd:simpleType>
        <xsd:simpleType name="UKPostcode">
                <xsd:restriction base="xsd:string">
                        <xsd:pattern value="[A-Z]{2}\d\s\d[A-Z]{2}"/>
                </xsd:restriction>
        </xsd:simpleType>

</xsd:schema>
  """

  // boeingData/ipo5/address.xsd
  val importedSchema = """
<schema xmlns="http://www.w3.org/2001/XMLSchema"
 xmlns:add="http://www.example.com/add" targetNamespace="http://www.example.com/add"
 elementFormDefault="qualified"
>
        <complexType name="AddressType">
                <sequence>
                        <element name="name" type="string"/>
                        <element name="street" type="string"/>
                        <element name="city" type="string"/>
                </sequence>
        </complexType>

</schema>
    """

  // boeingData/ipo5/itematt.xsd
  val includedSchema = """
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema">
        <xsd:attributeGroup name="ItemDelivery">
                <xsd:attribute name="partNum" type="SKU" use="required"/>
                <xsd:attribute name="weightKg" type="xsd:decimal"/>
                <xsd:attribute name="shipBy">
                        <xsd:simpleType>
                                <xsd:restriction base="xsd:string">
                                        <xsd:enumeration value="air"/>
                                        <xsd:enumeration value="land"/>
                                        <xsd:enumeration value="any"/>
                                </xsd:restriction>
                        </xsd:simpleType>
                </xsd:attribute>
        </xsd:attributeGroup>

        <xsd:simpleType name="SKU">
                <xsd:restriction base="xsd:string">
                        <xsd:pattern value="\d{3}-[A-Z]{2}"/>
                </xsd:restriction>
        </xsd:simpleType>
</xsd:schema>
    """


  object sut extends SchemaInstantiator with SchemaParser with SimpleSchemaStore with SchemaResolver with SchemaLoader {

    override def builtInSchemas: Iterable[Schema] = Seq(Schema.builtInSchema)

    override val xsdParsers = new XsdPushParserMod with XmlEventReaderInputs {}

    override def resolveImport(namespace: Namespace, importHint: SchemaImportHint): (Messages, Option[(xsdParsers.DriveInputs, Option[URI])]) = {
      if (namespace == Namespace("http://www.example.com/add")) {
        (emptyMessages, Some(xsdParsers.inputs(new StreamSource(new StringReader(importedSchema))), None))
      } else {
        (prepend(s"can not resolve schema for namespace: $namespace", emptyMessages), None)
      }
    }

    override def resolveInclude(schemaLocation: String, baseUri: Option[URI]): (Messages, Option[(xsdParsers.DriveInputs, Option[URI])]) = {
      if (schemaLocation == "itematt.xsd") {
        (emptyMessages, Some(xsdParsers.inputs(new StreamSource(new StringReader(includedSchema))), None))
      } else {
        (prepend(s"can not resolve schema for schema location: $schemaLocation", emptyMessages), None)
      }
    }

    def parse(schema: String): (Messages, Option[Schema]) = {
      val source = new StreamSource(new StringReader(schema))
      val inputs = xsdParsers.inputs(source)
      loadSchema(inputs, None)
    }

  }

  test("import") {
    inside(sut.parse(schema)) {
      case (Nil, Some(_)) =>
    }
  }

}
