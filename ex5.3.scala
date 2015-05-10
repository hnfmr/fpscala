import Stream._

object Exercise5_3 extends App {
  def takeWhile[A](s: => Stream[A])(p: A => Boolean): Stream[A] = {
    def helper(ss: => Stream[A], acc: Stream[A]): Stream[A] = {
      ss match {
        case Empty => acc
        case Cons(h, t) => if (p(h())) cons(h(), takeWhile(t())(p))
                           else acc
      }
    }
    helper(s, Empty)
  }

  val s1 = cons(4, cons(3, (cons(2, Empty))))
  val s2 = cons(4, cons(3, (cons(3, Empty))))
  println(takeWhile(s1)(x => x >= 3).toList)
  println(takeWhile(s2)(x => x >= 3).toList)

}
