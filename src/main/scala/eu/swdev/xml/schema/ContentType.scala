package eu.swdev.xml.schema

import eu.swdev.xml.base.SomeValue
import eu.swdev.xml.name.QName

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
  def withOccurs(o: Occurs): GroupParticle
}

sealed trait NestedParticle extends Particle

sealed trait ChoiceOrSeqParticle extends GroupParticle with NestedParticle

trait SeqAllEffectiveTotalRange { self: GroupParticle =>

  // 3.8.6.5
  override def effectiveTotalRange: Occurs = {
    val nestedRanges = nested.map(_.effectiveTotalRange)
    val min = occurs.min * nestedRanges.map(_.min).sum
    val max = occurs.max * nestedRanges.map(_.max).foldLeft(MaxOccurs.zero)((acc, o) => acc + o)
    Occurs(min, max)
  }

}

sealed case class SeqParticle(occurs: Occurs, nested: Seq[Particle]) extends ChoiceOrSeqParticle with SeqAllEffectiveTotalRange {
  override def withOccurs(o: Occurs) = copy(occurs = o)
}

sealed case class ChoiceParticle(occurs: Occurs, nested: Seq[Particle]) extends ChoiceOrSeqParticle {
  // 3.8.6.6
  override def effectiveTotalRange: Occurs = {
    val nestedRanges = nested.map(_.effectiveTotalRange)
    val min = occurs.min * nestedRanges.map(_.min).min
    val max = occurs.max * nestedRanges.map(_.max).foldLeft(MaxOccurs.zero)((acc, o) => acc max o)
    Occurs(min, max)
  }
  override def withOccurs(o: Occurs) = copy(occurs = o)
}

sealed case class AllParticle(occurs: Occurs, nested: Seq[Particle]) extends GroupParticle with SeqAllEffectiveTotalRange {
  override def withOccurs(o: Occurs) = copy(occurs = o)
}

case class ElemDecl(occurs: Occurs, name: QName, elemType: Type, nillable: Boolean, abstrct: Boolean, valueConstraint: Option[ValueConstraint], identityConstraints: Seq[IdentityConstraint]) extends NestedParticle with SchemaTopComponent {
  override def effectiveTotalRange: Occurs = occurs
}

case class ElemWildcard(occurs: Occurs, wildcard: Wildcard) extends NestedParticle {
  override def effectiveTotalRange: Occurs = occurs
}

// 3.8.6.5 & 3.8.6.6
case class EffectiveTotalRange(min: Int, max: MaxOccurs)

case class GroupDef(name: QName, particle: GroupParticle) extends SchemaTopComponent