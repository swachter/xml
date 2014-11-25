package eu.swdev.xml.schema

import eu.swdev.xml.name.QName
import org.scalatest.FunSuite

/**
  */
class TypeTest extends FunSuite {

  test("visitor") {

    val tv: TypeVisitor[QName, Unit] = new TypeVisitor[QName, Unit] {
      override def visit(tpe: anyType.type, p: Unit): QName = tpe.name
      override def visit(tpe: ComplexType, p: Unit): QName = tpe.name
      override def visit(tpe: anySimpleType.type, p: Unit): QName = tpe.name
      override def visit(tpe: ListType, p: Unit): QName = tpe.name
      override def visit(tpe: UnionType, p: Unit): QName = tpe.name
      override def visit(tpe: anyAtomicType.type, p: Unit): QName = tpe.name
      override def visit(tpe: untypedAtomicType.type, p: Unit): QName = tpe.name
      override def visit(tpe: BooleanType, p: Unit): QName = tpe.name
      override def visit(tpe: DoubleType, p: Unit): QName = tpe.name
      override def visit(tpe: DecimalType, p: Unit): QName = tpe.name
      override def visit(tpe: IntegerType, p: Unit): QName = tpe.name
      override def visit(tpe: LongType, p: Unit): QName = tpe.name
      override def visit(tpe: IntType, p: Unit): QName = tpe.name
      override def visit(tpe: StringType, p: Unit): QName = tpe.name
      override def visit(tpe: QNameType, p: Unit): QName = tpe.name
    }
    assert(anyType.accept(tv, ()) == XsNames.ANY_TYPE)
    assert(stringType.accept(tv, ()) == XsNames.STRING)


  }

}
