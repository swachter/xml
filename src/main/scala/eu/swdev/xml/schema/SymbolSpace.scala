package eu.swdev.xml.schema

import scala.reflect.ClassTag

sealed abstract class SymbolSpace[-X: ClassTag] {
  type SymbolType
  def name: String
  val symbolClass: Class[_] = implicitly[ClassTag[X]].runtimeClass
}

object SymbolSpace {
  implicit val Type = new SymbolSpace[Type] {
    type SymbolType = Type
    def name = "type"
  }
  implicit val ElemDecl = new SymbolSpace[ElemDecl] {
    type SymbolType = ElemDecl
    def name = "element declaration"
  }
  implicit val AttrDecl = new SymbolSpace[AttrDecl] {
    type SymbolType = AttrDecl
    def name = "attribute declaration"
  }
  implicit val Group = new SymbolSpace[Group] {
    type SymbolType = Group
    def name = "group"
  }
  implicit val AttrGroup = new SymbolSpace[AttrGroup] {
    type SymbolType = AttrGroup
    def name = "attribute group"
  }
  //implicit val IdentityConstraint extends SymbolSpace[IdentityConstraint]
  //implicit val Notation extends SymbolSpace[Notation]
}

