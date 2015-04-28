object Exercise2_5 extends App {
  def compose[A,B,C](f: B => C, g: A => B) : A => C = {
    a => f(g(a))
  }

  val addOne = (a:Int) => a+1
  val mulTwo = (a:Int) => a*2

  println(compose(addOne, mulTwo)(3))
}
