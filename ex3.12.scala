object Exercise3_12 extends App {
  @annotation.tailrec def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case x::xs => foldLeft(xs, f(z, x))(f)
    }

  def reverse[A](as: List[A]): List[A] = {
    def helper[A](z: List[A], a: A): List[A] = {
      a::z
    }

    foldLeft(as, List[A]())(helper)
  }

  val ls = List(1,2,3,4,5)
  println(reverse(ls).mkString(","))
}
