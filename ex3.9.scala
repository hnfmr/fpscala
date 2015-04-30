object Exercise3_9 extends App {
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case x::xs => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int = {
    def helper(a: A, c: Int): Int = {
      c+1
    }

    foldRight(as, 0)(helper)
  }

  val ls = List(1,2,3,4,5,7)
  println(length(ls))
}
