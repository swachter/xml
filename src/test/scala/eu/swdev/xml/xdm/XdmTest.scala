package eu.swdev.xml.xdm

import eu.swdev.xml.name.{LocalName, QNameFactory, Namespaces}
import eu.swdev.xml.schema._
import org.scalatest.FunSuite

/**
  */
class XdmTest extends FunSuite {

  test("value creation") {

    import Xdm._

    doubleType.createValue("5.0", Namespaces.initial): DoubleValue
    intType.createValue("5", Namespaces.initial)

    val intListType = ListType(QNameFactory.caching(LocalName("intList")), intType)

    intListType.createValue("5 6 7", Namespaces.initial)
  }
}
