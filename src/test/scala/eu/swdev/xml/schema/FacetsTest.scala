package eu.swdev.xml.schema

import eu.swdev.xml.schema.FacetsMod.Facets.HasDigits
import org.scalatest.FunSuite
import shapeless.HMap

/**
  */
class FacetsTest extends FunSuite {

  test("length facets") {

    import FacetsMod._

    val facets = new Facets[List[Int]](Map.empty)

    assert(facets.check(Nil, ""))

    val restricted = facets.length.set(2)

    assert(!restricted.check(Nil, ""))

    assert(restricted.check(1 :: 2 :: Nil, "1 2"))

//    assert(facets.Length.doCheck(Nil, 0))
//
//    assert(facets.Length.check(Nil))
//
//    val restricted = facets.MaxLength.restrict(3).MinLength.restrict(1)
//
//    assert(!restricted.MinLength.check(Nil))
//
//    assert(restricted.MaxLength.check(null :: Nil))
//
//    assert(restricted.MaxLength.check(Nil))
//
//    assert(!restricted.MaxLength.check(null :: null :: null :: null :: Nil))
//
//    val restricted2 = restricted.Length.restrict(2)


  }

  test("digits") {
    def totalDigits[X:HasDigits](x: X): Int = implicitly[HasDigits[X]].totalDigits(x)
    def fractionDigits[X:HasDigits](x: X): Int = implicitly[HasDigits[X]].fractionDigits(x)

    assert(totalDigits(BigDecimal("0")) == 1)
    assert(totalDigits(BigDecimal("0.0")) == 1)
    assert(totalDigits(BigDecimal("-0")) == 1)

    assert(totalDigits(BigDecimal("123")) == 3)
    assert(totalDigits(BigDecimal("123.00")) == 3)
    assert(totalDigits(BigDecimal("-123")) == 3)
    assert(totalDigits(BigDecimal("123.1")) == 4)
    assert(totalDigits(BigDecimal("123.10")) == 4)

    assert(fractionDigits(BigDecimal("0")) == 0)
    assert(fractionDigits(BigDecimal("123")) == 0)
    assert(fractionDigits(BigDecimal("-123")) == 0)
    assert(fractionDigits(BigDecimal("123.1")) == 1)
    assert(fractionDigits(BigDecimal("123.10")) == 1)
    assert(fractionDigits(BigDecimal("-123.1")) == 1)
    assert(fractionDigits(BigDecimal("-123.10")) == 1)

    assert(totalDigits(BigInt("0")) == 1)
    assert(totalDigits(BigInt("-0")) == 1)
    assert(totalDigits(BigInt("99")) == 2)
    assert(totalDigits(BigInt("-99")) == 2)


  }

}
