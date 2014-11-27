package eu.swdev.xml.name

import org.scalatest.FunSuite

class NamespacesTest extends FunSuite {

  test("namespace ctx") {
    val nsCtx = Namespaces.initial

    assert(nsCtx.namespaceForPrefix(NoPrefix).get == NoNamespace)
    assert(nsCtx.namespaceForPrefix(XmlPrefix).get == XmlNamespace)

    nsCtx.nestedScope(List((XmlPrefix -> XmlNamespace)))

    intercept[RuntimeException] {
      nsCtx.nestedScope(List((XmlPrefix -> XmlnsNamespace)))
    }

    val prefix = Prefix("p")
    val namespace = Namespace("ns")
    val nsCtx2 = nsCtx.nestedScope(List(prefix -> namespace))



    // check if newly declared namespace is available
    assert(nsCtx2.namespaceForPrefix(prefix).get == namespace)

    // check if namespace can be undeclared
    assert(nsCtx2.nestedScope(List(prefix -> NoNamespace)).namespaceForPrefix(prefix) == None)
  }

}
