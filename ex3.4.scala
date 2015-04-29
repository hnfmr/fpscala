object Exercise3_4 extends App {
  def drop[A](n: Int, ls: List[A]): List[A] = {
    n match {
      case 0 => ls
      case n if n > 0 => drop(n-1, ls.tail)
      case _ => sys.error("negative")
    }
  }

  val ls = List(1,2,3,4,5)
  println(drop(3,ls).mkString(","))

  println(drop(-1,ls).mkString(","))
}
