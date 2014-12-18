package eu.swdev.xml.schema

import java.net.URI

/**
  */
sealed trait ContentType {
  def simpleTypeDefinition: Option[SimpleType]
  def isMixedAndEmptiable: Boolean
  def isEmpty: Boolean
  def isSimple: Boolean
  def isElements: Boolean
}

sealed trait ComplexContentType extends ContentType

object EmptyContentType extends ComplexContentType {
  override def simpleTypeDefinition = None
  override def isMixedAndEmptiable = false
  override def isEmpty = true
  override def isSimple = false
  override def isElements = false
}

sealed case class SimpleContentType(tpe: SimpleType) extends ContentType {
  override def simpleTypeDefinition = Some(tpe)
  override def isMixedAndEmptiable = false
  override def isEmpty = false
  override def isSimple = true
  override def isElements = false
}

sealed case class ElementsContentType(group: GroupParticle, mixed: Boolean, open: Option[OpenContent]) extends ComplexContentType {
  override def simpleTypeDefinition = None
  override def isMixedAndEmptiable = mixed && group.isEmptiable
  override def isEmpty = false
  override def isSimple = false
  override def isElements = true
}

sealed trait Particle {
  def occurs: Occurs

  // 3.9.6.3
  def isEmptiable: Boolean = occurs.min == 0 || effectiveTotalRange.min == 0
  def effectiveTotalRange: Occurs
}

sealed trait GroupParticle extends Particle {
  def nested: Seq[Particle]
}

trait SeqAllEffectiveTotalRange { self: GroupParticle =>

  // 3.8.6.5
  override def effectiveTotalRange: Occurs = {
    val nestedRanges = nested.map(_.effectiveTotalRange)
    val min = occurs.min * nestedRanges.map(_.min).sum
    val max = occurs.max * nestedRanges.map(_.max).foldLeft(MaxOccurs.zero)((acc, o) => acc + o)
    Occurs(min, max)
  }

}

sealed case class SeqGroupParticle(occurs: Occurs, nested: Seq[Particle]) extends GroupParticle with SeqAllEffectiveTotalRange {
}

sealed case class ChoiceGroupParticle(occurs: Occurs, nested: Seq[Particle]) extends GroupParticle {
  // 3.8.6.6
  override def effectiveTotalRange: Occurs = {
    val nestedRanges = nested.map(_.effectiveTotalRange)
    val min = occurs.min * nestedRanges.map(_.min).min
    val max = occurs.max * nestedRanges.map(_.max).foldLeft(MaxOccurs.zero)((acc, o) => acc max o)
    Occurs(min, max)
  }
}

sealed case class AllGroupParticle(occurs: Occurs, nested: Seq[Particle]) extends GroupParticle with SeqAllEffectiveTotalRange {
}

case class ElemDecl()

// 3.8.6.5 & 3.8.6.6
case class EffectiveTotalRange(min: Int, max: MaxOccurs)
