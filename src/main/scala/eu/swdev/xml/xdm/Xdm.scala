package eu.swdev.xml.xdm

import eu.swdev.xml.base.WhitespaceProcessing
import eu.swdev.xml.name.Namespaces
import eu.swdev.xml.xsd._

import scala.util.Try

/**
  */
object Xdm {

//  def creationFunction[T <: Type](tpe: T)(implicit functions: ValueCreationFunctions[T]) = functions.creationFunction(tpe)
//
//  def createValue[T <: Type](tpe: T, string: String, ns: Namespaces)(implicit functions: ValueCreationFunctions[T]) = functions.creationFunction(tpe)(string, ns)
//
//  trait ValueCreationFunctions[-T] {
//    type V <: Value
//    def creationFunction(tpe: T): (String, Namespaces) => V
//  }

  private object creationFunction {

    def apply(tpe: untypedAtomicType.type ) = (string: String, ns: Namespaces) => UntypedAtomicValue(tpe.name, tpe.parse(string, ns))
    def apply(tpe: BooleanType) = (string: String, ns: Namespaces) => BooleanValue(tpe.name, tpe.parse(string, ns))
    def apply(tpe: DoubleType) = (string: String, ns: Namespaces) => DoubleValue(tpe.name, tpe.parse(string, ns))
    def apply(tpe: DecimalType) = (string: String, ns: Namespaces) => DecimalValue(tpe.name, tpe.parse(string, ns))
    def apply(tpe: IntegerType) = (string: String, ns: Namespaces) => IntegerValue(tpe.name, tpe.parse(string, ns))
    def apply(tpe: LongType) = (string: String, ns: Namespaces) => LongValue(tpe.name, tpe.parse(string, ns))
    def apply(tpe: IntType) = (string: String, ns: Namespaces) => IntValue(tpe.name, tpe.parse(string, ns))
    def apply(tpe: StringType) = (string: String, ns: Namespaces) => StringValue(tpe.name, tpe.parse(string, ns))
    def apply(tpe: QNameType) = (string: String, ns: Namespaces) => QNameValue(tpe.name, tpe.parse(string, ns))

    def apply(tpe: ListType): (String, Namespaces) => ListValue = {
      val createItemValueFunction: (String, Namespaces) => AtomicValue = tpe.itemType match {
        case Left(at) => at.createValue
        case Right(l) => {
          (string: String, ns: Namespaces) => l.iterator.map(at => Try { at.createValue(string, ns) }).find(_.isSuccess).map(_.get).getOrElse(throw new IllegalArgumentException(s"invalid value: $string; non of the union member types could parse the value"))
        }
      }
      (string: String, ns: Namespaces) => {
        val items = WhitespaceProcessing.Collapse.process(string).split(' ').map(s => createItemValueFunction(s, ns)).toList
        ListValue(tpe.name, items)
      }
    }

    def apply(tpe: UnionType): (String, Namespaces) => SimpleValue = (string, ns) => {
      tpe.memberTypes.iterator.map(at => Try { at.createValue(string, ns) }).find(_.isSuccess).map(_.get).getOrElse(throw new IllegalArgumentException(s"invalid value: $string; non of the union member types could parse the value"))
    }
  }

  implicit class SimpleTypeOps(val tpe: SimpleType) extends AnyVal {
    def createValue = tpe.accept(sv, ())
  }

  implicit class AtomicTypeOps(val tpe: AtomicType) extends AnyVal {
    def createValue = tpe.accept(av, ())
  }

  implicit class ListTypeOps(val tpe: ListType) extends AnyVal {
    def createValue = creationFunction(tpe)
  }

  implicit class UnionTypeOps(val tpe: UnionType) extends AnyVal {
    def createValue = creationFunction(tpe)
  }

  implicit class DoubleTypeOps(val tpe: DoubleType) extends AnyVal {
    def createValue = creationFunction(tpe)
  }

  implicit class LongTypeOps(val tpe: LongType) extends AnyVal {
    def createValue = creationFunction(tpe)
  }

  implicit class IntTypeOps(val tpe: IntType) extends AnyVal {
    def createValue = creationFunction(tpe)
  }

  trait AtomicValueCreatorVisitor {
    def visit(tpe: anyAtomicType.type, p: Unit): (String, Namespaces) => AtomicValue = throw new IllegalStateException("anyAtomicType is abstract")
    def visit(tpe: untypedAtomicType.type, p: Unit) = creationFunction(tpe)
    def visit(tpe: BooleanType, p: Unit) = creationFunction(tpe)
    def visit(tpe: DoubleType, p: Unit) = creationFunction(tpe)
    def visit(tpe: DecimalType, p: Unit) = creationFunction(tpe)
    def visit(tpe: IntegerType, p: Unit) = creationFunction(tpe)
    def visit(tpe: LongType, p: Unit) = creationFunction(tpe)
    def visit(tpe: IntType, p: Unit) = creationFunction(tpe)
    def visit(tpe: StringType, p: Unit) = creationFunction(tpe)
    def visit(tpe: QNameType, p: Unit) = creationFunction(tpe)
  }

  trait SimpleValueCreatorVisitor {
    def visit(tpe: anySimpleType.type, p: Unit): (String, Namespaces) => AtomicValue = throw new IllegalStateException("anySimpleType is abstract")
    def visit(tpe: UnionType, p: Unit) = creationFunction(tpe)
    def visit(tpe: ListType, p: Unit) = creationFunction(tpe)
  }

  object av extends AtomicTypeVisitor[(String, Namespaces) => AtomicValue, Unit] with AtomicValueCreatorVisitor
  object sv extends SimpleTypeVisitor[(String, Namespaces) => SimpleValue, Unit] with AtomicValueCreatorVisitor with SimpleValueCreatorVisitor


//  trait LowestPriorityValueCreators {
//    implicit val simpleValueCreator = new ValueCreationFunctions[SimpleType] {
//      override type V = SimpleValue
//      override def creationFunction(tpe: SimpleType) = tpe.accept(sv, ())
//    }
//  }
//  trait LowPriorityValueCreators extends LowestPriorityValueCreators {
//    implicit val atomicValueCreator = new ValueCreationFunctions[AtomicType] {
//      override type V = AtomicValue
//      override def creationFunction(tpe: AtomicType) = tpe.accept(av, ())
//    }
//  }
//
//  object ValueCreationFunctions extends LowPriorityValueCreators {
//    implicit val doubleValueCreator = new ValueCreationFunctions[DoubleType] {
//      override type V = DoubleValue
//      override def creationFunction(tpe: DoubleType) = creationFunction(tpe)
//    }
//  }

}
