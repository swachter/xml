package eu.swdev.xml.schema

import shapeless.HMap

import scala.util.matching.Regex


object FacetsMod {

  class KV[K, V]

  trait Facets[X] {

    trait Facet {
      type FV
      def doCheck(x: X, fv: FV): Boolean
    }

    def facets: HMap[KV]

    def check[F <: Facet](facet: F, x: X)(implicit ev: KV[F, facet.FV]): Boolean = facets.get(facet) match {
      case Some(fv) => facet.doCheck(x, fv)
      case None => true
    }
  }

  trait LengthFacets[X] {
    self: Facets[X] =>

    def getLength(x: X): Int

    object Length extends Facet {
      type FV = Int
      override def doCheck(x: X, fv: Int): Boolean = getLength(x) == fv
    }

    implicit val lkv = new KV[Length.type, Int]

    object MinLength extends Facet {
      type FV = Int
      override def doCheck(x: X, fv: Int): Boolean = getLength(x) >= fv
    }

    implicit val milkv = new KV[MinLength.type, Int]

    object MaxLength extends Facet {
      type FV = Int
      override def doCheck(x: X, fv: Int): Boolean = getLength(x) <= fv
    }

    implicit val malkv = new KV[MaxLength.type, Int]

  }

  trait GeneralFacets[X] {
    self: Facets[X] =>

    def getString(x: X): String

    object Patterns extends Facet {
      type FV = Seq[Regex]
      override def doCheck(x: X, fv: Seq[Regex]): Boolean = fv.exists(_.unapplySeq(getString(x)).isDefined)
    }

    implicit def patternsKv = new KV[Patterns.type, Seq[Regex]]

    object Enums extends Facet {
      type FV = Seq[X]
      override def doCheck(x: X, fv: Seq[X]): Boolean = fv.exists(_ == x)
    }

    implicit val enumsKv = new KV[Patterns.type, Seq[X]]
  }


  case class UnionFacets(facets: HMap[KV]) extends Facets[SimpleVal] with GeneralFacets[SimpleVal] {
    override def getString(x: SimpleVal): String = x.toString
  }

  case class ListFacets(facets: HMap[KV]) extends Facets[List[AtomicVal]] with GeneralFacets[List[AtomicVal]] with LengthFacets[List[AtomicVal]] {
    override def getString(x: List[AtomicVal]): String = x.toString
    override def getLength(x: List[AtomicVal]): Int = x.size
  }

  object ListFacets {
    val empty = ListFacets(new HMap())
  }

}


/*
case class GeneralFacets[E] (
  patterns: Seq[Regex],
  assertions: Seq[Assertion],
  enums: Seq[E]
)

object GeneralFacets {

  private val ev = GeneralFacets(Nil, Nil, Nil)

  def empty[E] = ev.asInstanceOf[GeneralFacets[E]]

}

case class LengthFacets (
  length: Option[Int],
  minLength: Option[Int],
  maxLength: Option[Int]
)

object LengthFacets {
  val empty = LengthFacets(None, None, None)
}

case class OrderFacets[R] (
  minExc: Option[R],
  minInc: Option[R],
  maxExc: Option[R],
  maxInc: Option[R]
)

object OrderFacets {

  private val ev = OrderFacets(None, None, None, None)

  def empty[R] = ev.asInstanceOf[OrderFacets[R]]

}

case class NumberFacets(
  totalDigits: Option[Int],
  fractionDigits: Option[Int]
)

object NumberFacets {
  val empty = NumberFacets(None, None)
}

*/