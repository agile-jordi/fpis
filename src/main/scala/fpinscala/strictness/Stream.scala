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

  def take(n: Int): List[A] = {

    @tailrec
    def loop(s:Stream[A], i:Int, acc:List[A]):List[A] = s match {
      case Cons(h,t) if i>0 => loop(t(),i-1,h() :: acc)
      case _ => acc.reverse
    }

    loop(this,n,List.empty[A])

  }

  def drop(n: Int): Stream[A] = {

    @tailrec
    def loop(s:Stream[A], i:Int):Stream[A] = s match {
      case Cons(h, t) if i > 0 => loop(t(),i-1)
      case _ => s
    }

    loop(this,n)
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h,t) =>
      val h1 = h()
      if(p(h1)) cons(h1,t().takeWhile(p))
      else Empty
  }
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