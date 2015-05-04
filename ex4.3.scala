import fpscala.errorhandling._

object Exercise4_3 extends App {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  def map2x[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(x), Some(y)) => Try(f(x,y))
    }
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
