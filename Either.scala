package fpscala.errorhandling

import scala.{Option => _, Either => _, _}
import scala.util.control.Exception._

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a) match {
        case Left(e) => Left(e)
        case Right(a) => Right(a)
      }
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => b
    case r@Right(_) => r
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }

}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

