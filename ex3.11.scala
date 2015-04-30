object Exercise3_11 extends App {
  @annotation.tailrec def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case x::xs => foldLeft(xs, f(z, x))(f)
    }

  def sum(as: List[Int]): Int = foldLeft(as, 0)(_ + _)

  def product(as: List[Int]): Int = foldLeft(as, 1)(_ * _)


  val ls = List(1,2,3,4,5)
  println("sum: " + sum(ls))
  println("product: " + product(ls))
}
