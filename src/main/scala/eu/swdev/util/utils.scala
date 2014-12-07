package eu.swdev

object util {

  def traverse[X, L, R](seq: List[X])(f: X => Either[L, R]): Either[L, List[R]] = seq match {
    case Nil => Right(Nil)
    case head :: tail => traverse(tail)(f) match {
      case Right(t) => f(head) match {
        case Right(h) => Right(h :: t)
        case Left(h) => Left(h)
      }
      case Left(t) => Left(t)
    }
  }
}

