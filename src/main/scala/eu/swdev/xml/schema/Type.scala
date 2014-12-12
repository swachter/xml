package eu.swdev.xml.schema

import java.util.concurrent.atomic.AtomicInteger

import eu.swdev.xml.base._
import eu.swdev.xml.name._
import eu.swdev.xml.schema.Facets.WhitespaceFacet

import scala.util.{Failure, Success, Try}

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
  type VAL <: SimpleVal
  def derivation = Relation.Restriction
  def facets: Facets[VAL]
  def createVal(lexicalRep: String, ns: Namespaces): Either[String, VAL]
  val accept: Accept[SimpleTypeVisitor]
}

sealed trait AtomicOrListType extends SimpleType {

}

sealed trait AtomicType extends AtomicOrListType {

  type Data
  type VAL = AtomicVal[Data]

  def whitespaceProcessing: WhitespaceProcessing = facets.whitespace.get.get.value

  val accept: Accept[AtomicTypeVisitor]

  final def parse(string: String, ns: Namespaces): Either[String, Data] = {
    doParse(whitespaceProcessing.process(string), ns)
  }

  final def createVal(lexicalRep: String, ns: Namespaces): Either[String, VAL] = parse(lexicalRep, ns).right.map(AtomicVal(this, _))

  def lexicalRep(data: Data): String

  protected def doParse(string: String, ns: Namespaces): Either[String, Data]

  protected def tryParse[X](parse: => X): Either[String, X] = Try { parse } match {
    case Success(x) => Right(x)
    case Failure(e) => Left(e.getMessage)
  }
}

sealed trait NonStringAtomicType extends AtomicType {
}

sealed case class UnionType(name: QName, baseType: SimpleType, facets: Facets[SimpleVal], memberTypes: List[AtomicOrListType]) extends SimpleType {
  self =>

  type VAL = SimpleVal

  require(!memberTypes.isEmpty, "a union type must have at least one member type")

  override def createVal(lexicalRep: String, ns: Namespaces): Either[String, VAL] = unionVal(memberTypes)(lexicalRep, ns)

  override val accept = new Accept[SimpleTypeVisitor] {
    override def apply[R, P](v: SimpleTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }

}

object UnionType {
  def apply(name: QName, baseType: SimpleType, facets: Facets[SimpleVal], memberTypes: Seq[SimpleType]): UnionType = {
    val mts = memberTypes.foldRight(List[AtomicOrListType]())((t, accu) => t match {
      case t: AtomicOrListType => t :: accu
      case UnionType(_, _, _, mt) => mt ++ accu
      case `anySimpleType` => throw new IllegalArgumentException("anySimpleType can not be a member type of a union")
    })
    new UnionType(name, baseType, facets, mts)
  }
}

/**
 *
 * @param itemType If the item type is an atomic type then Right[AtomicType] else the item type is a union type
 *                 then the list of its atomic types.
 */
sealed case class ListType(name: QName, baseType: SimpleType, facets: Facets[ListVal], itemType: Either[AtomicType, List[AtomicType]]) extends AtomicOrListType {
  self =>

  override type VAL = ListVal

  override def createVal(lexicalRep: String, ns: Namespaces): Either[String, VAL] = {
    val createItemValueFunction: String => Either[String, AtomicVal[_]] = itemType match {
      case Left(at) => s => at.createVal(s, ns)
      case Right(l) => s => unionVal(l)(s, ns)
    }
    val eitherItems = eu.swdev.util.traverse(WhitespaceProcessing.Collapse.process(lexicalRep).split(' ').toList)(createItemValueFunction)
    eitherItems.right.map(ListVal(this, _))
  }

  override val accept = new Accept[SimpleTypeVisitor] {
    override def apply[R, P](v: SimpleTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

object ListType {
  def apply(name: QName, baseType: SimpleType, facets: Facets[ListVal], itemType: SimpleType): ListType = {
    val either = itemType match {
      case t: AtomicType => Left(t)
      case t: ListType => throw new IllegalArgumentException("a list item type must not be a list type")
      case UnionType(_, _, _, memberTypes) => {
        if (memberTypes collect { case _: ListType => ()} isEmpty) {
          Right(memberTypes.asInstanceOf[List[AtomicType]])
        } else {
          throw new IllegalArgumentException("a union type that has a list type member can not be used as a list item type")
        }
      }
      case `anySimpleType` => throw new IllegalArgumentException("anySimpleType can not be a list item type")
    }
    new ListType(name, baseType, facets, either)
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
  override type VAL = Nothing
  override def facets = Facets.none
  final def createVal(lexicalRep: String, ns: Namespaces) = throw new IllegalStateException("anySimpleType is abstract")
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
  override def lexicalRep(data: Data): String = throw new IllegalStateException("anyAtomicType is abstract")
  override type VAL = AtomicVal[Nothing]
  override def facets = Facets.withWspPreserve

  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

object untypedAtomicType extends AtomicType {
  self =>
  override type Data = String
  override def name: QName = UNTYPED_ATOMIC
  override def baseType: Type = anyAtomicType
  override def doParse(string: String, ns: Namespaces): Either[String, Data] = Right(string)
  override def lexicalRep(data: Data): String = data
  override type VAL = AtomicVal[String]
  override def facets = Facets.withWspPreserve

  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class BooleanType(name: QName, baseType: Type, facets: Facets[AtomicVal[Boolean]]) extends NonStringAtomicType {
  self =>
  override type Data = Boolean
  override def doParse(string: String, ns: Namespaces): Either[String, Data] = if (string == "true" || string == "1") {
    Right(true)
  } else if (string == "false" || string == "0") {
    Right(false)
  } else {
    Left(s"invalid boolean value: $string")
  }
  override def lexicalRep(data: Data): String = String.valueOf(data)
  override type VAL = AtomicVal[Boolean]
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class DoubleType(name: QName, baseType: Type, facets: Facets[AtomicVal[Double]]) extends NonStringAtomicType {
  self =>
  override type Data = Double
  override def doParse(string: String, ns: Namespaces): Either[String, Data] = tryParse(string.toDouble)
  override def lexicalRep(data: Data): String = String.valueOf(data)
  override type VAL = AtomicVal[Double]
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class DecimalType(name: QName, baseType: Type, facets: Facets[AtomicVal[BigDecimal]]) extends NonStringAtomicType {
  self =>
  override type Data = BigDecimal
  override def doParse(string: String, ns: Namespaces): Either[String, Data] = tryParse(BigDecimal(string))
  override def lexicalRep(data: Data): String = data.toString()
  override type VAL = AtomicVal[BigDecimal]
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class IntegerType(name: QName, baseType: Type, facets: Facets[AtomicVal[BigInt]]) extends NonStringAtomicType {
  self =>
  override type Data = BigInt
  override def doParse(string: String, ns: Namespaces): Either[String, Data] = tryParse(BigInt(string))
  override def lexicalRep(data: Data): String = String.valueOf(data)
  override type VAL = AtomicVal[BigInt]
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class LongType(name: QName, baseType: Type, facets: Facets[AtomicVal[Long]]) extends NonStringAtomicType {
  self =>
  override type Data = Long
  override def doParse(string: String, ns: Namespaces): Either[String, Data] = tryParse(string.toLong)
  override def lexicalRep(data: Data): String = String.valueOf(data)
  override type VAL = AtomicVal[Long]
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class IntType(name: QName, baseType: Type, facets: Facets[AtomicVal[Int]]) extends NonStringAtomicType {
  self =>
  override type Data = Int
  override def doParse(string: String, ns: Namespaces): Either[String, Data] = tryParse(string.toInt)
  override def lexicalRep(data: Data): String = String.valueOf(data)
  override type VAL = AtomicVal[Int]
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class StringType(name: QName, baseType: Type, facets: Facets[AtomicVal[String]]) extends AtomicType {
  self =>
  override type Data = String
  override def doParse(string: String, ns: Namespaces): Either[String, Data] = Right(string)
  override def lexicalRep(data: Data): String = data
  override type VAL = AtomicVal[String]
  override val accept = new Accept[AtomicTypeVisitor] {
    override def apply[R, P](v: AtomicTypeVisitor[R, P], p: P): R = v.visit(self, p)
  }
}

sealed case class QNameType(name: QName, baseType: Type, facets: Facets[AtomicVal[QName]]) extends NonStringAtomicType {
  self =>
  override type Data = QName
  override def doParse(string: String, ns: Namespaces): Either[String, Data] = {
    val (opf, ln) = QName.parse(string)
    Right(QNameFactory.caching(opf.map(ns.namespaceForPrefix(_).get).getOrElse(NoNamespace), ln, opf.getOrElse(NoPrefix)))
  }
  override def lexicalRep(data: Data): String = String.valueOf(data)
  override type VAL = AtomicVal[QName]
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

