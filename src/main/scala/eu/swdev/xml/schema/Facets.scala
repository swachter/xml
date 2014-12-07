package eu.swdev.xml.schema

import scala.util.matching.Regex


class KV[+K, V]

case class Facets[X <: SimpleVal](underlying: Map[Facet, FacetValue[X, _]]) {

  def get[K <: Facet, V <: FacetValue[X, _]](k: K)(implicit ev: KV[K, V]): Option[V] = underlying.get(k).asInstanceOf[Option[V]]

  def +[K <: Facet, V <: FacetValue[X, _]](kv: (K, V))(implicit ev: KV[K, V]) = Facets[X](underlying + kv)

  def check(x: X, lexicalRep: String): Boolean = underlying.values.forall(_.check(x, lexicalRep))

}

trait Facet

trait FacetValue[X, V] {
  def check(x: X, lexicalRep: String): Boolean

  def isRestriction(v: V): Boolean
}

object Facets {

  val none = Facets[Nothing](Map.empty)

  def empty[X <: SimpleVal]: Facets[X] = none.asInstanceOf[Facets[X]]

  case class FacetOp[F <: Facet, X <: SimpleVal, V, FV <: FacetValue[X, V]](facets: Facets[X], facet: F, createFacetValue: V => FV) {

    implicit val kv = new KV[F, FV]

    def set(value: V): Facets[X] = facets +(facet, createFacetValue(value))

    def isRestriction(value: V): Boolean = facets.get(facet) match {
      case Some(fv) => fv.isRestriction(value)
      case None => true
    }

  }

  implicit class OrderFacetsOps[X <: SimpleVal](facets: Facets[X])(implicit ev: Ordering[X]) {

    case class MinIncFacetValue(value: X) extends FacetValue[X, X] {
      override def check(x: X, lexicalRep: String): Boolean = ev.lteq(value, x)
      override def isRestriction(v: X): Boolean = ev.gteq(value, v)
    }

    case class MinExcFacetValue(value: X) extends FacetValue[X, X] {
      override def check(x: X, lexicalRep: String): Boolean = ev.lt(value, x)
      override def isRestriction(v: X): Boolean = ev.gteq(value, v)
    }

    case class MaxIncFacetValue(value: X) extends FacetValue[X, X] {
      override def check(x: X, lexicalRep: String): Boolean = ev.gteq(value, x)
      override def isRestriction(v: X): Boolean = ev.lteq(value, v)
    }

    case class MaxExcFacetValue(value: X) extends FacetValue[X, X] {
      override def check(x: X, lexicalRep: String): Boolean = ev.gt(value, x)
      override def isRestriction(v: X): Boolean = ev.lteq(value, v)
    }

    def minInc = FacetOp(facets, MinIncFacet, MinIncFacetValue.apply)
    def minExc = FacetOp(facets, MinExcFacet, MinExcFacetValue.apply)
    def maxInc = FacetOp(facets, MaxIncFacet, MaxIncFacetValue.apply)
    def maxExc = FacetOp(facets, MaxExcFacet, MaxExcFacetValue.apply)

  }

  implicit class LengthFacetsOps[X <: SimpleVal](facets: Facets[X])(implicit ev: HasLength[X]) {

    case class LengthFacetValue(value: Int) extends FacetValue[X, Int] {
      override def check(x: X, lexicalRep: String): Boolean = ev.length(x) == value
      override def isRestriction(v: Int): Boolean = v == value
    }

    case class MinLengthFacetValue(value: Int) extends FacetValue[X, Int] {
      override def check(x: X, lexicalRep: String): Boolean = ev.length(x) >= value
      override def isRestriction(v: Int): Boolean = v <= value
    }

    case class MaxLengthFacetValue(value: Int) extends FacetValue[X, Int] {
      override def check(x: X, lexicalRep: String): Boolean = ev.length(x) <= value
      override def isRestriction(v: Int): Boolean = v >= value
    }

    def length = FacetOp(facets, LengthFacet, LengthFacetValue.apply)
    def minLength = FacetOp(facets, MinLengthFacet, MinLengthFacetValue.apply)
    def maxLength = FacetOp(facets, MaxLengthFacet, MaxLengthFacetValue.apply)

  }

  implicit class DigitFacetsOps[X <: SimpleVal](facets: Facets[X])(implicit ev: HasDigits[X]) {

    case class TotalDigitsFacetValue(value: Int) extends FacetValue[X, Int] {
      override def check(x: X, lexicalRep: String): Boolean = ev.totalDigits(x) == value
      override def isRestriction(v: Int): Boolean = v == value
    }

    case class FractionDigitsFacetValue(value: Int) extends FacetValue[X, Int] {
      override def check(x: X, lexicalRep: String): Boolean = ev.fractionDigits(x) >= value
      override def isRestriction(v: Int): Boolean = v <= value
    }

    def totalDigits = FacetOp(facets, TotalDigitsFacet, TotalDigitsFacetValue.apply)
    def fractionDigits = FacetOp(facets, FractionDigitsFacet, FractionDigitsFacetValue.apply)

  }

  implicit class ExplicitTimeZoneOp[X <: SimpleVal](facets: Facets[X])(implicit ev: HasTimeZone[X]) {

    case class ExplicitTimeZoneFacetValue(value: ExplicitTimeZone) extends FacetValue[X, ExplicitTimeZone] {
      override def check(x: X, lexicalRep: String): Boolean = value match {
        case ExplicitTimeZone.Optional => true
        case ExplicitTimeZone.Prohibited => !ev.hasTimeZone(x)
        case ExplicitTimeZone.Required => ev.hasTimeZone(x)
      }
      override def isRestriction(v: ExplicitTimeZone): Boolean = (value, v) match {
        case (ExplicitTimeZone.Prohibited, ExplicitTimeZone.Prohibited) => true
        case (ExplicitTimeZone.Prohibited, _) => false
        case (ExplicitTimeZone.Required, ExplicitTimeZone.Required) => true
        case (ExplicitTimeZone.Required, _) => false
        case _ => true
      }
    }

    def explicitTimeZone = FacetOp(facets, ExplicitTimeZoneFacet, ExplicitTimeZoneFacetValue.apply)

  }

  implicit class GeneralFacetsOps[X <: SimpleVal](facets: Facets[X]) {

    case class EnumFacetValue(value: Seq[X]) extends FacetValue[X, Seq[X]] {
      override def check(x: X, lexicalRep: String): Boolean = value.exists(_ == x)
      override def isRestriction(r: Seq[X]): Boolean = r.forall(s => value.exists(_ == s))
    }

    case class PatternFacetValue(value: Seq[Regex]) extends FacetValue[X, Seq[Regex]] {
      override def check(x: X, lexicalRep: String): Boolean = value.exists(_.unapplySeq(lexicalRep).isDefined)
      override def isRestriction(r: Seq[Regex]): Boolean = true
    }

    def enum = FacetOp(facets, EnumFacet, EnumFacetValue.apply)
    def pattern = FacetOp(facets, PatternFacet, PatternFacetValue.apply)
  }


  trait HasLength[X] {
    def length(x: X): Int
  }

  object HasLength {

    implicit val listValHasLength = new HasLength[ListVal] {
      override def length(x: ListVal): Int = x.data.size
    }

    implicit val stringHasLength = new HasLength[String] {
      override def length(x: String): Int = x.length
    }

    implicit def atomicValueHasLength[X](implicit ev: HasLength[X]) = new HasLength[AtomicVal[X]] {
      override def length(x: AtomicVal[X]): Int = ev.length(x.data)
    }

  }

  trait HasDigits[X] {
    def totalDigits(x: X): Int

    def fractionDigits(x: X): Int
  }

  object HasDigits {

    implicit val bigDecimalHasDigits = new HasDigits[BigDecimal] {
      override def totalDigits(x: BigDecimal): Int = x.bigDecimal.stripTrailingZeros.precision
      override def fractionDigits(x: BigDecimal): Int = x.bigDecimal.stripTrailingZeros.scale
    }

    trait NoFractionDigits[X] extends HasDigits[X] {
      final def fractionDigits(x: X) = 0
    }

    implicit val bigIntHasDigits = new NoFractionDigits[BigInt] {
      override def totalDigits(x: BigInt): Int = BigDecimal(x).precision
    }

    implicit val longHasDigits = new NoFractionDigits[Long] {
      override def totalDigits(x: Long): Int = BigDecimal(x).precision
    }

    implicit val intHasDigits = new NoFractionDigits[Int] {
      override def totalDigits(x: Int): Int = BigDecimal(x).precision
    }

    implicit val shortHasDigits = new NoFractionDigits[Short] {
      override def totalDigits(x: Short): Int = BigDecimal(x).precision
    }

    implicit val charHasDigits = new NoFractionDigits[Char] {
      override def totalDigits(x: Char): Int = BigDecimal(x).precision
    }

    implicit val byteHasDigits = new NoFractionDigits[Byte] {
      override def totalDigits(x: Byte): Int = BigDecimal(x).precision
    }

    implicit def atomicValueHasDigits[X](implicit ev: HasDigits[X]) = new HasDigits[AtomicVal[X]] {
      override def totalDigits(x: AtomicVal[X]): Int = ev.totalDigits(x.data)
      override def fractionDigits(x: AtomicVal[X]): Int = ev.fractionDigits(x.data)
    }

  }

  trait HasTimeZone[X] {
    def hasTimeZone(x: X): Boolean
  }

  object HasTimeZone {

    implicit def atomicValueHasTimeZone[X](implicit ev: HasTimeZone[X]) = new HasTimeZone[AtomicVal[X]] {
      override def hasTimeZone(x: AtomicVal[X]): Boolean = ev.hasTimeZone(x.data)
    }

  }

  object LengthFacet extends Facet

  object MinLengthFacet extends Facet

  object MaxLengthFacet extends Facet

  object TotalDigitsFacet extends Facet

  object FractionDigitsFacet extends Facet

  object ExplicitTimeZoneFacet extends Facet

  object MinIncFacet extends Facet

  object MinExcFacet extends Facet

  object MaxIncFacet extends Facet

  object MaxExcFacet extends Facet

  object EnumFacet extends Facet

  object PatternFacet extends Facet

}

