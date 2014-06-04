package fpinscala.errors

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(aa => b.map(bb => f(aa,bb)))
}

case class Left[+E](value: E) extends Either[E, Nothing] {

  override def map[B](f: (Nothing) => B) = this

  override def flatMap[EE >: E, B](f: (Nothing) => Either[EE, B]) = this

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]) = b
}

case class Right[+A](value: A) extends Either[Nothing, A] {

  override def map[B](f: (A) => B) = Right(f(value))

  override def flatMap[EE >: Nothing, B](f: (A) => Either[EE, B]) = f(value)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]) = this

}

object Either {
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
}