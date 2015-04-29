object Exercise3_2 extends App {
  def myTail[A](xs: List[A]): List[A] = {
    xs match {
      case Nil => Nil
      case _::t => t
    }
  }


  val ls = List(1,2,3,4)
  println(myTail(ls).mkString(","))

  println(myTail(Nil).mkString(","))
}
