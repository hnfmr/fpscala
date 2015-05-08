package fpscala.errorhandling

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def map2[A, B, C](a: Either[List[String], A], b: Either[List[String], B])(f: (A, B) => C): Either[List[String], C] = {
    a match {
      case Left(es) => b match {
        case Left(bs) => Left(es ++ bs)
        case _ => Left(es)
      }
      case Right(aa) => b match {
        case Left(bs) => Left(bs)
        case Right(bb) => Right(f(aa, bb))
      }
    }
  }

  def mkName(name: String): Either[List[String], Name] =
    if (name == "" || name == null) Left(List("Name is empty."))
    else Right(new Name(name))

  def mkAge(age: Int): Either[List[String], Age] =
    if (age < 0) Left(List("Age is out of range."))
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[List[String], Person] =
    map2(mkName(name), mkAge(age))(Person(_, _))
}


