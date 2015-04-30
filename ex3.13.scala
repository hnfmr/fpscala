object Exercise3_13 extends App {
  @annotation.tailrec def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case x::xs => foldLeft(xs, f(z, x))(f)
    }

  def flip[A,B](f: (A,B) => B): (B,A) => B = (b, a) => f(a, b)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)(flip(f))

  val ls = List(1,2,3,4,5)
  println(foldRight(ls, 0)(_ + _))
  println(foldRight(List[Int](), 0)(_ + _))
}
