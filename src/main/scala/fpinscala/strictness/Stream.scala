package fpinscala.strictness

import scala.annotation.tailrec

sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = foldRight[Option[A]](None)((elem,_) => Some(elem))

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

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((elem,acc) => if(p(elem)) cons(elem,acc) else empty[A])

  def exists(p: A => Boolean): Boolean = foldRight(false)((elem, b) => p(elem) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((elem,b) => p(elem) && b)

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((elem,acc) => cons(f(elem),acc))

  def filter(p: A => Boolean):Stream[A] = foldRight(empty[A])((elem,acc) => if(p(elem)) cons(elem,acc) else acc)

  def append[B >: A](s2: => Stream[B]):Stream[B] = foldRight(s2)((elem,acc) => cons(elem,acc))

  def flatMap[B](f: A => Stream[B]):Stream[B] = foldRight(empty[B])((elem,acc) => f(elem).append(acc))
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

  def constant[A](a: A): Stream[A] = cons(a,constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs = {
    def fibs(prev2:Int,prev1:Int):Stream[Int] = cons(prev2+prev1, fibs(prev1, prev2+prev1))
    cons(0,cons(1,fibs(0,1)))
  }
}