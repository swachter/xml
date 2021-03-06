package eu.swdev.xml.name

import org.scalatest.{Inside, FunSuite}

class QNameTest extends FunSuite with Inside {

  test("QName extractor") {

    val Ns = Namespace("ns")
    val Ln = LocalName("ln")
    val Pf = Prefix("pf")

    val qn = QNameFactory.simple(Ns, Ln, Pf)

    inside(qn) {
      case QName(Ns, Ln, Pf) =>
    }

    inside(qn) {
      case QName(ns, ln, pf) => {
        assert(qn.namespace == ns)
        assert(qn.localName == ln)
        assert(qn.prefix == pf)
      }
    }
  }

}
