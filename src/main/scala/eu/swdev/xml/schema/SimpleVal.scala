package eu.swdev.xml.schema

trait SimpleVal

case class AtomicVal[X](tpe: AtomicType, data: X) extends SimpleVal

object AtomicVal {
  implicit def ordering[X: Ordering]: Ordering[AtomicVal[X]] = new Ordering[AtomicVal[X]] {
    override def compare(x: AtomicVal[X], y: AtomicVal[X]): Int = implicitly[Ordering[X]].compare(x.data, y.data)
  }
}

case class ListVal(tpe: ListType, data: List[AtomicVal[_]]) extends SimpleVal

