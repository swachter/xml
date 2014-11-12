package eu.swdev.xml.name

case class Namespace(val underlying: String) extends AnyVal

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
      (None, new LocalName(lexicalRep))
    } else if (idx > 0) {
      (Some(new Prefix(lexicalRep.substring(0, idx))), new LocalName(lexicalRep.substring(idx + 1)))
    } else {
      throw new IllegalArgumentException(s"invalid QName: $lexicalRep")
    }
  }
}
