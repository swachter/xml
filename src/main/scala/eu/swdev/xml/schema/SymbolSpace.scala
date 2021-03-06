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
  implicit val Group = new SymbolSpace[GroupDef] {
    type SymbolType = GroupDef
    def name = "group"
  }
  implicit val AttrGroup = new SymbolSpace[AttrGroup] {
    type SymbolType = AttrGroup
    def name = "attribute group"
  }
  implicit val IdentityConstraint = new SymbolSpace[IdentityConstraint] {
    type SymbolType = IdentityConstraint
    def name = "identity constraint"
  }
  implicit val Notation = new SymbolSpace[Notation] {
    type SymbolType = Notation
    def name = "notation"
  }
}

