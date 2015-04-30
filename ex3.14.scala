object Exercise3_14 extends App {
  @annotation.tailrec def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case x::xs => foldLeft(xs, f(z, x))(f)
    }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    def helper[A](as: List[A], a: A): List[A] = {
      a::as
    }
    foldLeft(a1.reverse, a2)(helper)
  }

  println(append(List(1,2,3,4), List(5,6,7,8)).mkString(","))
}
