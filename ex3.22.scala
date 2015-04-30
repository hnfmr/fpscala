object Exercise3_22 extends App {
  def sumTwo[A](a1: List[Int], a2: List[Int]): List[Int] = {
    def helper(as: List[Int], bs: List[Int], acc: List[Int]): List[Int] =
      (a1, a2) match {
        case (Nil, Nil) => acc
        case (ah::at, bh::bt) => helper(at, bt, (ah+bh)::acc)
      }

    helper(a1, a2, List[Int]())
  }

  val as = List[Int](1,2,3,4)
  val bs = List[Int](5,6,7,8)
  println(sumTwo(as, bs).mkString(","))
}
