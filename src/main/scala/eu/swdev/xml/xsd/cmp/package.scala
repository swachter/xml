package eu.swdev.xml.xsd

import eu.swdev.xml.name.QName

/**
  */
package object cmp {

  type KeyRefOrDef[KD <: KeyDefCmp] = Either[QName, KD]
}
