object Exercise3_16 extends App {
  def map[A,B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil => Nil
      case h::t => f(h) :: map(t)(f)
    }

  val ls = List(1,2,3,4,5)
  println(map(ls)(_ + 1).mkString(","))
}
