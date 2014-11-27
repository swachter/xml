package eu.swdev.xml.name

import java.net.URI

case class Namespace(val underlying: String) extends AnyVal

object Namespace {
  val NoNamespace = Namespace("")

  def apply(ou: Option[URI]): Namespace = ou match {
    case Some(u) => Namespace(u)
    case None => NoNamespace
  }

  def apply(u: URI): Namespace = Namespace(u.toString)

}

case class LocalName(val underlying: String) extends AnyVal

case class Prefix(val underlying: String) extends AnyVal

trait QName {
  def namespace: Namespace
  def localName: LocalName
  def prefix: Prefix
}

object QName {
  def unapply(qName: QName) = Some((qName.namespace, qName.localName, qName.prefix))

  def parse(lexicalRep: String): (Option[Prefix], LocalName) = {
    val idx = lexicalRep.indexOf(':')
    if (idx < 0) {
      (None, LocalName(lexicalRep))
    } else if (idx > 0) {
      (Some(Prefix(lexicalRep.substring(0, idx))), LocalName(lexicalRep.substring(idx + 1)))
    } else {
      throw new IllegalArgumentException(s"invalid QName: $lexicalRep")
    }
  }
}
