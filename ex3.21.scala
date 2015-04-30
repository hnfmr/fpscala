object Exercise3_21 extends App {
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

  def map[A,B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil => Nil
      case h::t => f(h) :: map(t)(f)
    }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    val lls = map(as)(f)
    flatten(lls)
  }


  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(x => if (f(x)) List(x) else Nil)
  }

  val ls = List(1,2,3,4,5,6,7,8)

  println(filter(ls)(_ % 2 == 0).mkString(","))
}
