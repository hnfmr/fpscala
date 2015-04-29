
object Exercise3_1 extends App {
  val x = List(1,2,3,4,5) match {
    // case Cons(x, Cons(2, Cons(4, _))) => x
    case x :: 2 :: 4 :: _ => x
    case Nil => 42
    // case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case x :: y :: 3 :: 4 :: _ => x + y
    // case Cons(h, t) => h + sum(t)
    case h :: t => h + t.sum
    case _ => 101
  }
  println(x)
}
