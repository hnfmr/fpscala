object Exercise3_6 extends App {
  def init[A](ls: List[A]): List[A] = {
    def helper(acc: List[A], as: List[A]): List[A] = {
      as match {
        case Nil => acc
        case h::t => t match {
                       case Nil => acc.reverse
                       case _::tt => helper(h::acc, t)
                     }
      }
    }

    helper(List(), ls)
  }

  val ls = List(5,4,3,2,1)
  println(init(ls).mkString(","))
}
