object Exercise3_19 extends App {
  def filterH[A](as: List[A])(f: A => Boolean): List[A] = {
    def helper(acc: List[A], ls: List[A]): List[A] =
      ls match {
        case Nil => acc
        case h::t => if (f(h)) helper(h::acc, t) else helper(acc, t)
      }
    helper(List[A](), as)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    filterH(as)(f).reverse
  }

  val ls = List(1,2,3,4,5)
  println(filter(ls)(_ % 2 == 0).mkString(","))
}
