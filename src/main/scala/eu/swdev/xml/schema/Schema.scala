package eu.swdev.xml.schema

import eu.swdev.xml.name.{LocalName, Namespace}

/**
  */
case class Schema(namespace: Namespace, types: Map[LocalName, Type], elements: Map[LocalName, ElemDecl], attrs: Map[LocalName, AttrDecl])