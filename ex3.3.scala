object Exercise3_3 extends App {
  def setHead[A](a: A, as: List[A]): List[A] = {
    as match {
      case Nil => Nil
      case _::t => a::t
    }
  }


  val ls = List(1,2,3,4)
  println(setHead(0,ls).mkString(","))

  println(setHead(0,Nil).mkString(","))
}
