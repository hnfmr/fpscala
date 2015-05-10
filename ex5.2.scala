import Stream._

object Exercise5_2 extends App {
  def take[A](s: => Stream[A], n: Int): Stream[A] = {
    def helper(ss: => Stream[A], nn: Int, acc: Stream[A]): Stream[A] = {
      if (nn == 0) acc
      else ss match {
        case Empty => Empty
        case Cons(h, t) => helper(t(), nn-1, cons(h(), acc))
      }
    }
    helper(s, n, Empty)
  }

  def drop[A](s: => Stream[A], n: Int): Stream[A] = {
    if (n <= 0) s
    else s match {
      case Empty => Empty
      case Cons(h, t) => if (n == 1) t()
      else drop(t(), n - 1)
    }
  }

  val ss = cons(1, cons(2, (cons(3, Empty))))
  println(take(ss, 1).toList)

  println(drop(ss, 2).toList)
  println(drop(ss, 0).toList)
  println(drop(ss, -1).toList)
}
