import fpscala.errorhandling._

object Exercise4_7 extends App {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es match {
      case Nil => Right(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    sequence(as map (a => f(a)))


  val as = List(Right(10), Right(11), Right(12))
  println(sequence(as))

  val bs = List(Right(17), Left(new Exception()), Right(11), Right(12))
  println(sequence(bs))

  println(traverse(as)(x => {
    if (x == 10) Left(new Exception())
    else Right(x)
  }))
}
