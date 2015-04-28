object Exercise2_4 extends App {
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a,b) => f(a)(b)
  }

  val f = (a:Int) => (b:Int) => a + b
  val r = uncurry(f)(10,2)
  println(r)
}
