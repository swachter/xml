package eu.swdev.xml.name

trait QNameFactory {
  def apply(localName: LocalName): QName = apply(NoNamespace, localName, NoPrefix)
  def apply(namespace: Namespace, localName: LocalName): QName = apply(namespace, localName, NoPrefix)
  def apply(namespace: Namespace, localName: LocalName, prefix: Prefix): QName
}

object QNameFactory {

  // A QName factory that reuses QNames
  val caching: QNameFactory = new QNameFactory {
    import java.util.concurrent.ConcurrentHashMap
    private val cache = new ConcurrentHashMap[QName, QName]()
    override def apply(namespace: Namespace, localName: LocalName, prefix: Prefix): QName = {
      val qn = new QNameImpl(namespace, localName, prefix)
      val previous = cache.putIfAbsent(qn, qn)
      if (previous != null) previous else qn
    }
  }

  // A QName factory that always returns a new QName
  val simple: QNameFactory = new QNameFactory {
    override def apply(namespace: Namespace, localName: LocalName, prefix: Prefix): QName = new QNameImpl(namespace, localName, prefix)
  }

  private class QNameImpl(val namespace: Namespace, val localName: LocalName, val prefix: Prefix) extends QName {

    override def hashCode(): Int = namespace.hashCode + 17 * localName.hashCode

    override def equals(other: Any): Boolean = other match {
      case o: QName => namespace == o.namespace && localName == o.localName
      case _ => false
    }

    override def toString: String = s"QName($namespace,$localName,$prefix)"
  }


}