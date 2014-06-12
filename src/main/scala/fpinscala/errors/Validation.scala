package fpinscala.errors

import fpinscala.datastructures.List

sealed trait Validation[+E, +A] {

  def map[B](f: A => B): Validation[E, B]

  def flatMap[EE >: E, B](f: A => Validation[EE, B]): Validation[EE, B]

  def orElse[EE >: E, B >: A](b: => Validation[EE, B]): Validation[EE, B]

  def map2[EE >: E, B, C](b: Validation[EE, B])(f: (A, B) => C): Validation[EE, C]

}

case class Error[+E](errors: List[E]) extends Validation[E, Nothing] {

  override def map[B](f: (Nothing) => B) = this

  override def flatMap[EE >: E, B](f: (Nothing) => Validation[EE, B]) = this

  override def orElse[EE >: E, B >: Nothing](b: => Validation[EE, B]) = b match {
    case Error(errs) => Error(List.append(errors, errs))
    case ok@Ok(_) => ok
  }

  override def map2[EE >: E, B, C](b: Validation[EE, B])(f: (Nothing, B) => C) = b match {
    case Error(bErrors) => Error(List.append(errors, bErrors))
    case _ => this
  }
}

case class Ok[+A](value: A) extends Validation[Nothing, A] {

  override def map[B](f: (A) => B) = Ok(f(value))

  override def flatMap[EE >: Nothing, B](f: (A) => Validation[EE, B]) = f(value)

  override def orElse[EE >: Nothing, B >: A](b: => Validation[EE, B]) = this

  override def map2[EE >: Nothing, B, C](b: Validation[EE, B])(f: (A, B) => C) = b match {
    case Ok(bv) => Ok(f(value, bv))
    case Error(bErrors) => Error(bErrors)
  }
}
