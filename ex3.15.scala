object Exercise3_15 extends App {
  @annotation.tailrec def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case x::xs => foldLeft(xs, f(z, x))(f)
    }

  def flip[A,B](f: (A,B) => B): (B,A) => B = (b, a) => f(a, b)

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)(flip(f))
  
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    def helper[A](as: List[A], a: A): List[A] = {
      a::as
    }
    foldLeft(a1.reverse, a2)(helper)
  }

  def flatten[A](aas: List[List[A]]): List[A] = {
    aas match {
      case Nil => Nil
      case h::t => append(h, flatten(t))
    }
  }

  val lls = List(List(1,2,3), List(4,5,6), List(7,8,9))
  println(flatten(lls).mkString(","))
}
