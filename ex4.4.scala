import fpscala.errorhandling._

object Exercise4_4 extends App {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def helper(aa: List[Option[A]], acc: Option[List[A]]): Option[List[A]] =
      aa match {
        case Nil => acc
        case ah::at => ah match {
          case None => None
          case Some(x) => helper(at, acc.flatMap(xs => Some(x::xs)))
        }
      }

    if (a == Nil) None
    else helper(a, Some(List[A]()))
  }
  
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }

  val b = List(None, Some(10), Some(11), Some(9))
  val bs = sequence(b)
  println(bs)

  val a = List(Some(1), Some(2))
  val as = sequence(a)
  println(as)
}
