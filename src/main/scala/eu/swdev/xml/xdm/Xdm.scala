package eu.swdev.xml.xdm

import eu.swdev.xml.base.WhitespaceProcessing
import eu.swdev.xml.name.Namespaces
import eu.swdev.xml.schema._

import scala.util.Try

/**
  */
object Xdm {

  private object createValueFunctions {

    def apply(tpe: untypedAtomicType.type ) = (string: String, ns: Namespaces) => tpe.parse(string, ns).right.map(UntypedAtomicValue(tpe.name, _))
    def apply(tpe: BooleanType) = (string: String, ns: Namespaces) => tpe.parse(string, ns).right.map(BooleanValue(tpe.name, _))
    def apply(tpe: DoubleType) = (string: String, ns: Namespaces) => tpe.parse(string, ns).right.map(DoubleValue(tpe.name, _))
    def apply(tpe: DecimalType) = (string: String, ns: Namespaces) => tpe.parse(string, ns).right.map(DecimalValue(tpe.name, _))
    def apply(tpe: IntegerType) = (string: String, ns: Namespaces) => tpe.parse(string, ns).right.map(IntegerValue(tpe.name, _))
    def apply(tpe: LongType) = (string: String, ns: Namespaces) => tpe.parse(string, ns).right.map(LongValue(tpe.name, _))
    def apply(tpe: IntType) = (string: String, ns: Namespaces) => tpe.parse(string, ns).right.map(IntValue(tpe.name, _))
    def apply(tpe: StringType) = (string: String, ns: Namespaces) => tpe.parse(string, ns).right.map(StringValue(tpe.name, _))
    def apply(tpe: QNameType) = (string: String, ns: Namespaces) => tpe.parse(string, ns).right.map(QNameValue(tpe.name, _))

    def apply(tpe: ListType) = (lexicalRep: String, ns: Namespaces) =>  {
      val createItemValueFunction: String => Either[String, AtomicValue] = tpe.itemType match {
        case Left(at) => s => at.createValue(s, ns)
        case Right(l) => s => unionValue[AtomicType, AtomicValue](l)((at: AtomicType) => at.accept(av, ()))(s, ns)
      }
      val eitherItems = eu.swdev.util.traverse(WhitespaceProcessing.Collapse.process(lexicalRep).split(' ').toList)(createItemValueFunction)
      eitherItems.right.map(ListValue(tpe.name, _))
    }

    def apply(tpe: UnionType): (String, Namespaces) => Either[String, SimpleValue] = unionValue[AtomicOrListType, SimpleValue](tpe.memberTypes)((st: SimpleType) => st.accept(sv, ()))

    def unionValue[T <: AtomicOrListType, V <: SimpleValue](memberTypes: List[T])(cvf: T => (String, Namespaces) => Either[String, V])(lexicalRep: String, ns: Namespaces): Either[String, V] =
      memberTypes.iterator.map(at => Try { cvf(at)(lexicalRep, ns) }).find(_.isSuccess).map(_.get).getOrElse(throw new IllegalArgumentException(s"invalid value: $lexicalRep; non of the union member types could parse the value"))
  }

  implicit class SimpleTypeOps(val tpe: SimpleType) extends AnyVal {
    def createValue = tpe.accept(sv, ())
  }

  implicit class AtomicTypeOps(val tpe: AtomicType) extends AnyVal {
    def createValue = tpe.accept(av, ())
  }

  implicit class ListTypeOps(val tpe: ListType) extends AnyVal {
    def createValue = createValueFunctions(tpe)
  }

  implicit class UnionTypeOps(val tpe: UnionType) extends AnyVal {
    def createValue = createValueFunctions(tpe)
  }

  implicit class DoubleTypeOps(val tpe: DoubleType) extends AnyVal {
    def createValue = createValueFunctions(tpe)
  }

  implicit class LongTypeOps(val tpe: LongType) extends AnyVal {
    def createValue = createValueFunctions(tpe)
  }

  implicit class IntTypeOps(val tpe: IntType) extends AnyVal {
    def createValue = createValueFunctions(tpe)
  }

  trait AtomicVisitMethods {
    def visit(tpe: anyAtomicType.type, p: Unit): (String, Namespaces) => Either[String, AtomicValue] = (_, _) => Left("anyAtomicType is abstract")
    def visit(tpe: untypedAtomicType.type, p: Unit) = createValueFunctions(tpe)
    def visit(tpe: BooleanType, p: Unit) = createValueFunctions(tpe)
    def visit(tpe: DoubleType, p: Unit) = createValueFunctions(tpe)
    def visit(tpe: DecimalType, p: Unit) = createValueFunctions(tpe)
    def visit(tpe: IntegerType, p: Unit) = createValueFunctions(tpe)
    def visit(tpe: LongType, p: Unit) = createValueFunctions(tpe)
    def visit(tpe: IntType, p: Unit) = createValueFunctions(tpe)
    def visit(tpe: StringType, p: Unit) = createValueFunctions(tpe)
    def visit(tpe: QNameType, p: Unit) = createValueFunctions(tpe)
  }

  trait SimpleVisitMethods {
    def visit(tpe: anySimpleType.type, p: Unit): (String, Namespaces) => Either[String, SimpleValue] = (_, _) => Left("anySimpleType is abstract")
    def visit(tpe: UnionType, p: Unit) = createValueFunctions(tpe)
    def visit(tpe: ListType, p: Unit) = createValueFunctions(tpe)
  }

  object av extends AtomicTypeVisitor[(String, Namespaces) => Either[String, AtomicValue], Unit] with AtomicVisitMethods
  object sv extends SimpleTypeVisitor[(String, Namespaces) => Either[String, SimpleValue], Unit] with AtomicVisitMethods with SimpleVisitMethods


}
