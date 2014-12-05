package eu.swdev.xml.schema

import shapeless.HMap

import scala.util.matching.Regex


object FacetsMod {

  class KV[+K, V]

  case class Facets[X](underlying: Map[Facet, FacetValue[X]]) {

    def get[K <: Facet, V <: FacetValue[X]](k : K)(implicit ev : KV[K, V]) : Option[V] = underlying.get(k).asInstanceOf[Option[V]]

    def +[K <: Facet, V <: FacetValue[X]](kv: (K, V))(implicit ev: KV[K, V]) = Facets[X](underlying + kv)

    def check(x: X): Boolean = underlying.values.forall(_.check(x))

  }

  trait Facet

  trait FacetValue[X] {
    type V
    def check(x: X): Boolean
    def isRestriction(v: V): Boolean
  }

  abstract class FacetOp[F <: Facet, X](facets: Facets[X], facet: F) {

    type FV <: FacetValue[X]

    implicit val kv = new KV[F, FV]

    def set(value: FV#V): Facets[X] = facets + (facet, createFacetValue(value))

    def createFacetValue(value: FV#V): FV

  }

  implicit class LengthFacetOps[X : HasLength](facets: Facets[X]) {

    case class LengthFacetValue(value: Int) extends FacetValue[X] {
      type V = Int
      override def check(x: X): Boolean = implicitly[HasLength[X]].getLength(x) == value
      override def isRestriction(v: Int): Boolean = v == value
    }

    def length = new FacetOp(facets, LengthFacet) {

      override type FV = LengthFacetValue

      override def createFacetValue(value: Int): FV = new LengthFacetValue(value)
    }


  }


  trait HasLength[X] {
    def getLength(x: X): Int
  }

  object HasLength {
    implicit def listHasLength[X] = new HasLength[List[X]] {
      override def getLength(x: List[X]): Int = x.size
    }
  }

  object LengthFacet extends Facet



//  trait Facet {
//
//    type FV
//
//    implicit val kv = new KV[this.type, FV]
//
//    def doCheck(x: X, fv: FV): Boolean
//
//    def check(x: X, hMap: HMap[KV]): Boolean = hMap.get(this) match {
//      case Some(fv) => doCheck(x, fv)
//      case None => true
//    }
//
//    def restrict(fv: FV) = create(hMap + (this, fv))
//  }
//
//
//
//  trait Facets[X] {
//
//    type FACETS
//
//    def hMap: HMap[KV]
//
//    def create(facets: HMap[KV]): FACETS
//
//  }

//  trait LengthFacets[X] {
//    self: Facets[X] =>
//
//    def getLength(x: X): Int
//
//    object Length extends Facet {
//      type FV = Int
//      override def doCheck(x: X, fv: Int): Boolean = getLength(x) == fv
//    }
//
//    object MinLength extends Facet {
//      type FV = Int
//      override def doCheck(x: X, fv: Int): Boolean = getLength(x) >= fv
//    }
//
//    object MaxLength extends Facet {
//      type FV = Int
//      override def doCheck(x: X, fv: Int): Boolean = getLength(x) <= fv
//    }
//
//  }
//
//  trait GeneralFacets[X] {
//    self: Facets[X] =>
//
//    def getString(x: X): String
//
//    object Patterns extends Facet {
//      type FV = Seq[Regex]
//      override def doCheck(x: X, fv: Seq[Regex]): Boolean = fv.exists(_.unapplySeq(getString(x)).isDefined)
//    }
//
//    object Enums extends Facet {
//      type FV = Seq[X]
//      override def doCheck(x: X, fv: Seq[X]): Boolean = fv.exists(_ == x)
//    }
//
//  }
//
//
//  case class UnionFacets(hMap: HMap[KV]) extends Facets[SimpleVal] with GeneralFacets[SimpleVal] {
//    type FACETS = UnionFacets
//    override def getString(x: SimpleVal): String = x.toString
//    override def create(facets: HMap[KV]) = UnionFacets(facets)
//  }
//
//  case class ListFacets(hMap: HMap[KV]) extends Facets[List[AtomicVal]] with GeneralFacets[List[AtomicVal]] with LengthFacets[List[AtomicVal]] {
//    type FACETS = ListFacets
//    override def getString(x: List[AtomicVal]): String = x.toString
//    override def getLength(x: List[AtomicVal]): Int = x.size
//    override def create(facets: HMap[KV]) = ListFacets(facets)
//  }
//
//  object ListFacets {
//    val empty = ListFacets(new HMap())
//  }

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