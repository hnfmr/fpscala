import fpscala.errorhandling._
import fpscala.errorhandling.Person._

object Exercise4_8 extends App {
  val p = mkPerson("", -1)
  p match {
    case Left(e) => println("Error: " ++ e.mkString(","))
    case Right(v) => println("Person: " ++ v.toString)
  }
}
