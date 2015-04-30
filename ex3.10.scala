object Exercise3_10 extends App {
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case x::xs => f(x, foldRight(xs, z)(f))
    }

  @annotation.tailrec def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case x::xs => foldLeft(xs, f(z, x))(f)
    }

  val ls = List(1,2,3,4,5)
  val left = foldLeft(ls, 0)(_ + _)
  val right = foldRight(ls, 0)(_ + _)

  println(left)
  println(right)
}
