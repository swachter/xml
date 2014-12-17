package eu.swdev.xml.name

/**
 * Provides information about in scope namespace declarations.
 *
 * In all methods an empty namespace string corresponds to the "no namespace"
 * and an empty prefix string corresponds to "no prefix".
 *
 * Every namespace context knows the built-in "xml"-namespace.
 */
class Namespaces(val underlying: Map[Prefix, Namespace]) extends AnyVal {

  def namespaceForPrefix(prefix: Prefix): Option[Namespace] = underlying.get(prefix)

  def defaultNamespace: Namespace = underlying(NoPrefix)

  def nestedScope(namespaceDecls: TraversableOnce[(Prefix, Namespace)]): Namespaces =
    new Namespaces(namespaceDecls.foldLeft(underlying)((map, nsDecl) => nsDecl match {
      case (XmlPrefix, XmlNamespace) => map
      case (XmlPrefix, n) => throw new RuntimeException("xml prefix can not be bound to a different namespace - namespace: $n")
      case (p, XmlNamespace) => throw new RuntimeException(s"the xml namespace must not be bound to a prefix other than xml - prefix: $p")

      case (XmlnsPrefix, _) => throw new RuntimeException("the xmlns prefix can not be bound")
      case (_, XmlnsNamespace) => throw new RuntimeException("the xmlns namespace can not be bound")

      case d @ (NoPrefix, ns) => map + d // set default namespace
      case (prefix, NoNamespace) => map - prefix // undeclare prefix
      case d => map + d // add prefix -> namespace mapping
    }))

}

object Namespaces {
  val initial = new Namespaces(Map(NoPrefix -> NoNamespace, XmlPrefix -> XmlNamespace))
}