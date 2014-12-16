package eu.swdev.etc

case class HMap[R[_, _]](underlying: Map[Any, Any]) {

  def get[K, V](k: K)(implicit ev: R[K, V]): Option[V] = underlying.get(k).asInstanceOf[Option[V]]

  def +[K, V](kv: (K, V))(implicit ev: R[K, V]): HMap[R] = new HMap(underlying + kv)

}

object HMap {

  def empty[R[_, _]]: HMap[R] = new HMap[R](Map.empty)
}