package eu.swdev.xml.base

/**
  */
case class ResId(publicId: Option[PublicId], systemId: Option[SystemId])

case class PublicId(val string: String) extends AnyVal
case class SystemId(val string: String) extends AnyVal

case class Location(resId: ResId, row: Int, col: Int)
