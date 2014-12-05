package eu.swdev.xml.schema

import eu.swdev.xml.name.Namespaces

trait SimpleVal

trait AtomicVal {
  type TPE <: AtomicType
  def tpe: TPE
  def data: TPE#Data
}

//object AtomicVal {
//
//  def apply(lexicalRep: String, namespaces: Namespaces, tpe: AtomicType): AtomicVal = {
//    new AtomicVal(tpe)(tpe.parse(lexicalRep, namespaces))
//  }
//
//}

class ListVal(tpe: ListType)(data: List[AtomicVal]) extends SimpleVal

//object ListVal {
//
//  def apply(lexicalRep: String, namespaces: Namespaces, tpe: ListType): ListVal = {
//    tpe.itemType match {
//      case Left(at) =>
//    }
//  }
//
//}