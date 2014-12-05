package eu.swdev.xml.schema

import java.util.concurrent.atomic.AtomicInteger

import eu.swdev.xml.base.{True, False, Unknown, Ternary}
import eu.swdev.xml.name._

/**
 *
 */
sealed trait Type {

  /**
   * The unique name of the type. Anonymous types get synthetic names.
   * @return
   */
  def name: QName

  def optBaseType: Option[Type]

  /**
   * Checks if this type is a super type of the specified type. Note that a type
   * is a super type of itself.
   */
  final def isSupertypeOf(tpe: Type): Boolean = {
    if (this == tpe) {
      true
    } else {
      tpe.optBaseType.map(isSupertypeOf(_)).getOrElse(false)
    }
  }

  /**
   * Checks if this type is a subtype of the specified type. Note that a type is a
   * subtype of itself.
   */
  def isSubtypeOf(tpe: Type): Boolean

  /**
   * Checks if this type is an atomic type. In case that this type is a super type
   * of atomic types (e.g. this type is the anyType or anySimpleType)
   * Unknown is returned.
   */
  //def isAtomicType: Ternary

  /**
   * Checks if this type is a numeric type. Numeric types are double, float, decimal,
   * and all types derived from decimal. In case that this type is a super type
   * of one of the numeric types (e.g. this type is the anyType or anySimpleType)
   * Unknown is returned.
   */
  //def isNumericType: Ternary

  /**
   * The accept method is represented by a value that is overloaded in derived types.
   * The accept method in sub types may require less capable visitors.
   */
  val accept: Accept[TypeVisitor]

}

sealed trait DerivedType extends Type {

  def optBaseType = Some(baseType)

  def baseType: Type

  def derivation: Relation

  final def isSubtypeOf(tpe: Type): Boolean = {
    if (this == tpe) {
      true
    } else {
      baseType.isSubtypeOf(tpe)
    }
  }
}

sealed case class ComplexType(
  name: QName,
  baseType: Type,
  derivation: CtDerivationCtrl,
  attrs: AttrsModel,
  var content: ContentModel,
  isAbstract: Boolean,
  finalSet: Set[CtDerivationCtrl],
  prohibitedSubstitutions: Set[CtDerivationCtrl],
  assertions: List[Assertion]
) extends DerivedType {
  self =>
  override val accept: Accept[TypeVisitor] = new Accept[TypeVisitor] {
    override def apply[R, P](v: TypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed trait Assertion

sealed trait SimpleType extends DerivedType {
  def derivation = Relation.Restriction

  val accept: Accept[SimpleTypeVisitor]
}

sealed trait AtomicOrListType extends SimpleType {

}

sealed trait AtomicType extends AtomicOrListType {

  type Data

  def whitespaceFacet: WhitespaceFacet

  val accept: Accept[AtomicTypeVisitor]

  final def parse(string: String, ns: Namespaces): Data = {
    doParse(whitespaceFacet.whitespaceProcess.process(string), ns)
  }

  def doParse(string: String, ns: Namespaces): Data

}

sealed trait NonStringAtomicType extends AtomicType {
  override def whitespaceFacet: WhitespaceFacet = WhitespaceFacet.COLLAPSE_FIXED
}

sealed case class UnionType(name: QName, memberTypes: List[AtomicOrListType]) extends SimpleType {
  self =>
  override def baseType = anySimpleType
  require(!memberTypes.isEmpty, "a union type must have at least one member type")
  override val accept = new Accept[SimpleTypeVisitor] {
    override def apply[R, P](v: SimpleTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

object UnionType {
  def apply(name: QName, memberTypes: Seq[SimpleType]): UnionType = {
    val mts = memberTypes.foldRight(List[AtomicOrListType]())((t, accu) => t match {
      case t: AtomicOrListType => t :: accu
      case UnionType(_, mt) => mt ++ accu
      case `anySimpleType` => throw new IllegalArgumentException("anySimpleType can not be a member type of a union")
    })
    new UnionType(name, mts)
  }
}

/**
 *
 * @param itemType If the item type is an atomic type then Right[AtomicType] else the item type is a union type
 *                 then the list of its atomic types.
 */
sealed case class ListType(name: QName, itemType: Either[AtomicType, List[AtomicType]]) extends AtomicOrListType {
  self =>
  override def baseType = anySimpleType
  override val accept = new Accept[SimpleTypeVisitor] {
    override def apply[R, P](v: SimpleTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

object ListType {
  def apply(name: QName, itemType: SimpleType): ListType = {
    val either = itemType match {
      case t: AtomicType => Left(t)
      case t: ListType => throw new IllegalArgumentException("a list item type must not be a list type")
      case UnionType(_, memberTypes) => {
        if (memberTypes collect { case _: ListType => ()} isEmpty) {
          Right(memberTypes.asInstanceOf[List[AtomicType]])
        } else {
          throw new IllegalArgumentException("a union type that has a list type member can not be used as a list item type")
        }
      }
      case `anySimpleType` => throw new IllegalArgumentException("anySimpleType can not be a list item type")
    }
    new ListType(name, either)
  }
}

import XsNames._

//
// Built-in types
//

object anyType extends Type {
  self =>
  override def name: QName = ANY_TYPE
  override def optBaseType: Option[Type] = None
  override def isSubtypeOf(tpe: Type): Boolean = this == tpe
  override val accept = new Accept[TypeVisitor] {
    override def apply[R, P](v: TypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

object anySimpleType extends SimpleType {
  self =>
  override def name: QName = ANY_SIMPLE_TYPE
  override def baseType: Type = anyType
  override val accept = new Accept[SimpleTypeVisitor] {
    override def apply[R, P](v: SimpleTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

object anyAtomicType extends AtomicType {
  self =>
  override type Data = Nothing
  override def name: QName = ANY_ATOMIC_TYPE
  override def baseType: Type = anySimpleType
  override def doParse(string: String, ns: Namespaces): Data = throw new IllegalStateException("anyAtomicType is abstract")

  override def whitespaceFacet: WhitespaceFacet = WhitespaceFacet.PRESERVE_UNFIXED

  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

object untypedAtomicType extends AtomicType {
  self =>
  override type Data = String
  override def name: QName = UNTYPED_ATOMIC
  override def baseType: Type = anyAtomicType
  override def doParse(string: String, ns: Namespaces): Data = string

  override def whitespaceFacet: WhitespaceFacet = WhitespaceFacet.PRESERVE_UNFIXED

  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class BooleanType(name: QName, baseType: Type) extends NonStringAtomicType {
  self =>
  override type Data = Boolean
  override def doParse(string: String, ns: Namespaces): Data = if (string == "true" || string == "1") {
    true
  } else if (string == "false" || string == "0") {
    false
  } else {
    throw new IllegalArgumentException(s"invalid boolean value: $string")
  }
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class DoubleType(name: QName, baseType: Type) extends NonStringAtomicType {
  self =>
  override type Data = Double
  override def doParse(string: String, ns: Namespaces): Data = string.toDouble
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class DecimalType(name: QName, baseType: Type) extends NonStringAtomicType {
  self =>
  override type Data = BigDecimal
  override def doParse(string: String, ns: Namespaces): Data = BigDecimal(string)
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class IntegerType(name: QName, baseType: Type) extends NonStringAtomicType {
  self =>
  override type Data = BigInt
  override def doParse(string: String, ns: Namespaces): Data = BigInt(string)
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class LongType(name: QName, baseType: Type) extends NonStringAtomicType {
  self =>
  override type Data = Long
  override def doParse(string: String, ns: Namespaces): Data = string.toLong
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class IntType(name: QName, baseType: Type) extends NonStringAtomicType {
  self =>
  override type Data = Int
  override def doParse(string: String, ns: Namespaces): Data = string.toInt
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class StringType(name: QName, baseType: Type, whitespaceFacet: WhitespaceFacet) extends AtomicType {
  self =>
  override type Data = String
  override def doParse(string: String, ns: Namespaces): Data = string
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class QNameType(name: QName, baseType: Type) extends NonStringAtomicType {
  self =>
  override type Data = QName
  override def doParse(string: String, ns: Namespaces): Data = {
    val (opf, ln) = QName.parse(string)
    QNameFactory.caching(opf.map(ns.namespaceForPrefix(_).get).getOrElse(NoNamespace), ln, opf.getOrElse(NoPrefix))
  }
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

//
//
//

trait AtomicTypeVisitor[Result, Param] {
  def visit(tpe: anyAtomicType.type, p: Param): Result

  def visit(tpe: untypedAtomicType.type, p: Param): Result

  def visit(tpe: BooleanType, p: Param): Result

  def visit(tpe: DoubleType, p: Param): Result

  def visit(tpe: DecimalType, p: Param): Result

  def visit(tpe: IntegerType, p: Param): Result

  def visit(tpe: LongType, p: Param): Result

  def visit(tpe: IntType, p: Param): Result

  def visit(tpe: StringType, p: Param): Result

  def visit(tpe: QNameType, p: Param): Result
}

trait SimpleTypeVisitor[Result, Param] extends AtomicTypeVisitor[Result, Param] {
  def visit(tpe: anySimpleType.type, p: Param): Result

  def visit(tpe: ListType, p: Param): Result

  def visit(tpe: UnionType, p: Param): Result
}

trait TypeVisitor[Result, Param] extends SimpleTypeVisitor[Result, Param] {
  def visit(tpe: anyType.type, p: Param): Result

  def visit(tpe: ComplexType, p: Param): Result
}

/**
 * Represents the accept method.
 *
 * Accept methods have a contravariant type parameter for the type visitor. It is contravariant because an accept method
 * that needs a "less capable" type visitor is a sub type of an accept method that needs a more capable type visitor.
 * In other words: Accept[AtomicTypeVisitor] <: Accept[TypeVisitor]
 *
 * @tparam TV
 */
trait Accept[-TV[_, _]] {
  def apply[R, P](v: TV[R, P], p: P): R
}

