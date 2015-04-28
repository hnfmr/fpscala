object Exercise2_3 extends App {
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  val f = (a:Int, b:Int) => a + b
  println(curry(f)(10)(20))
}

