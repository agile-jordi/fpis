package fpinscala.strictness

import scala.annotation.tailrec

sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = {

    @tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc.reverse
      case Cons(h, t) => loop(t(), h() :: acc)
    }

    loop(this, List.empty[A])
  }

  def take(n: Int): Stream[A] = this match {
    case Empty => Empty
    case _ if n <= 0 => Empty
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
  }


  def drop(n: Int): Stream[A] = {

    @tailrec
    def loop(s: Stream[A], i: Int): Stream[A] = s match {
      case Cons(h, t) if i > 0 => loop(t(), i - 1)
      case _ => s
    }

    loop(this, n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      val h1 = h()
      if (p(h1)) cons(h1, t().takeWhile(p))
      else Empty
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((elem, b) => p(elem) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((elem,b) => p(elem) && b)
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
}