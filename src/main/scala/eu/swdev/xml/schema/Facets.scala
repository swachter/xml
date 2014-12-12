package eu.swdev.xml.schema

import eu.swdev.xml.base.WhitespaceProcessing

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
  def restrict(v: V): Either[String, V]
}

object Facets {

  val none = Facets[Nothing](Map.empty)

  def empty[X <: SimpleVal]: Facets[X] = none.asInstanceOf[Facets[X]]

  def withWspPreserve[X <: SimpleVal]: Facets[X] = empty[X].whitespace.checkAndSet(WhitespaceProcessing.Preserve).right.get

  def withWspCollapse[X <: SimpleVal]: Facets[X] = empty[X].whitespace.checkAndSet(WhitespaceProcessing.Collapse).right.get

  /**
   * Operation for checking if setting a facet value would be a restriction and setting that value.
   *
   * @param facets
   * @param facet
   * @param createFacetValue
   * @tparam F
   * @tparam X
   * @tparam V
   * @tparam FV
   */
  case class FacetOp[F <: Facet, X <: SimpleVal, V, FV <: FacetValue[X, V]](facets: Facets[X], facet: F, createFacetValue: V => FV) {

    implicit val kv = new KV[F, FV]

    def checkAndSet(value: V): Either[String, Facets[X]] = 
      facets.get(facet).fold[Either[String, V]](Right(value))(_.restrict(value)).right.map(set(_))
    
    private def set(value: V): Facets[X] = facets + (facet, createFacetValue(value))

    def get: Option[FV] = facets.get(facet)

  }

  implicit class OrderFacetsOps[X <: SimpleVal](facets: Facets[X])(implicit ev: Ordering[X]) {

    case class MinIncFacetValue(value: X) extends FacetValue[X, X] {
      override def check(x: X, lexicalRep: String): Boolean = ev.lteq(value, x)
      override def restrict(v: X): Either[String, X] = if (ev.gteq(value, v)) Right(v) else Left(s"min. inclusive can not be restricted from $value to $v")
    }

    case class MinExcFacetValue(value: X) extends FacetValue[X, X] {
      override def check(x: X, lexicalRep: String): Boolean = ev.lt(value, x)
      override def restrict(v: X): Either[String, X] = if (ev.gteq(value, v)) Right(v) else Left(s"min. exclusive can not be restricted from $value to $v")
    }

    case class MaxIncFacetValue(value: X) extends FacetValue[X, X] {
      override def check(x: X, lexicalRep: String): Boolean = ev.gteq(value, x)
      override def restrict(v: X): Either[String, X] = if (ev.lteq(value, v)) Right(v) else Left(s"max. inclusive can not be restricted from $value to $v")
    }

    case class MaxExcFacetValue(value: X) extends FacetValue[X, X] {
      override def check(x: X, lexicalRep: String): Boolean = ev.gt(value, x)
      override def restrict(v: X): Either[String, X] = if (ev.lteq(value, v)) Right(v) else Left(s"max. exclusive can not be restricted from $value to $v")
    }

    def minInc = FacetOp(facets, MinIncFacet, MinIncFacetValue.apply)
    def minExc = FacetOp(facets, MinExcFacet, MinExcFacetValue.apply)
    def maxInc = FacetOp(facets, MaxIncFacet, MaxIncFacetValue.apply)
    def maxExc = FacetOp(facets, MaxExcFacet, MaxExcFacetValue.apply)

  }

  implicit class LengthFacetsOps[X <: SimpleVal](facets: Facets[X])(implicit ev: HasLength[X]) {

    case class LengthFacetValue(value: Int) extends FacetValue[X, Int] {
      override def check(x: X, lexicalRep: String): Boolean = ev.length(x) == value
      override def restrict(v: Int): Either[String, Int] = if (v == value) Right(v) else Left(s"length can not be restricted form $value to $v")
    }

    case class MinLengthFacetValue(value: Int) extends FacetValue[X, Int] {
      override def check(x: X, lexicalRep: String): Boolean = ev.length(x) >= value
      override def restrict(v: Int): Either[String, Int] = if (v <= value) Right(v) else Left(s"minimum length can not be restricted form $value to $v")
    }

    case class MaxLengthFacetValue(value: Int) extends FacetValue[X, Int] {
      override def check(x: X, lexicalRep: String): Boolean = ev.length(x) <= value
      override def restrict(v: Int): Either[String, Int] = if (v >= value) Right(v) else Left(s"maximum length can not be restricted form $value to $v")
    }

    def length = FacetOp(facets, LengthFacet, LengthFacetValue.apply)
    def minLength = FacetOp(facets, MinLengthFacet, MinLengthFacetValue.apply)
    def maxLength = FacetOp(facets, MaxLengthFacet, MaxLengthFacetValue.apply)

  }

  implicit class DigitFacetsOps[X <: SimpleVal](facets: Facets[X])(implicit ev: HasDigits[X]) {

    case class TotalDigitsFacetValue(value: Int) extends FacetValue[X, Int] {
      override def check(x: X, lexicalRep: String): Boolean = ev.totalDigits(x) == value
      override def restrict(v: Int): Either[String, Int] = if (v == value) Right(v) else Left(s"total digits can not be restricted form $value to $v")
    }

    case class FractionDigitsFacetValue(value: Int) extends FacetValue[X, Int] {
      override def check(x: X, lexicalRep: String): Boolean = ev.fractionDigits(x) >= value
      override def restrict(v: Int): Either[String, Int] = if (v <= value) Right(v) else Left(s"fraction digits can not be restricted form $value to $v")
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
      override def restrict(v: ExplicitTimeZone): Either[String, ExplicitTimeZone] = (value, v) match {
        case (ExplicitTimeZone.Prohibited, ExplicitTimeZone.Prohibited) => Right(v)
        case (ExplicitTimeZone.Prohibited, _) => Left(s"explicit timezone can not be restricted from $value to $v")
        case (ExplicitTimeZone.Required, ExplicitTimeZone.Required) => Right(v)
        case (ExplicitTimeZone.Required, _) => Left(s"explicit timezone can not be restricted from $value to $v")
        case _ => Left(s"explicit timezone can not be restricted from $value to $v")
      }
    }

    def explicitTimeZone = FacetOp(facets, ExplicitTimeZoneFacet, ExplicitTimeZoneFacetValue.apply)

  }

  implicit class GeneralFacetsOps[X <: SimpleVal](facets: Facets[X]) {

    case class EnumFacetValue(value: Seq[X]) extends FacetValue[X, Seq[X]] {
      override def check(x: X, lexicalRep: String): Boolean = value.exists(_ == x)
      override def restrict(v: Seq[X]): Either[String, Seq[X]] = if (v.forall(s => value.exists(_ == s))) Right(v) else Left(s"enumeration facet can not be restricted from $value to $v")
    }

    case class PatternFacetValue(value: Seq[Regex]) extends FacetValue[X, Seq[Regex]] {
      override def check(x: X, lexicalRep: String): Boolean = value.exists(_.unapplySeq(lexicalRep).isDefined)
      override def restrict(v: Seq[Regex]): Either[String, Seq[Regex]] = Right(v)
    }

    case class WhitespaceFacetValue(value: WhitespaceProcessing) extends FacetValue[X, WhitespaceProcessing] {
      override def check(x: X, lexicalRep: String): Boolean = true
      override def restrict(v: WhitespaceProcessing): Either[String, WhitespaceProcessing] = if (value.isSameOrRestriction(v)) Right(v) else Left(s"whitespace facet can not be restricted from $value to $v")
    }

    def enum = FacetOp(facets, EnumFacet, EnumFacetValue.apply)
    def pattern = FacetOp(facets, PatternFacet, PatternFacetValue.apply)
    def whitespace = FacetOp(facets, WhitespaceFacet, WhitespaceFacetValue.apply)
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

  object WhitespaceFacet extends Facet

}

