object Exercise3_5 extends App {
  def dropWhile[A](f: A => Boolean, ls: List[A]): List[A] = {
    ls match {
      case Nil => ls
      case h::t => if (f(h)) dropWhile(f, t) else ls
    }
  }

  val ls = List(5,4,3,2,1)
  println(dropWhile( (x:Int) => if (x > 3) true else false, ls).mkString(","))
}
