import fpscala.errorhandling._

object Exercise4_4 extends App {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft(None)((x,
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (aa => b map (bb => f(aa, bb)))
  }

  val ao = Some(10)
  val bo = Some(11)
  println(map2(ao, bo)((a:Int, b:Int) => a + b))

  val bx = None
  println(map2(ao, bx)((a:Int, b:Int) => a + b))
}
