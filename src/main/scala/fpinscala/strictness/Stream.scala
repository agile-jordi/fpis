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

  def takeFoldRight(n: Int): Stream[A] = this match {
    case Empty => Empty
    case _ if n <= 0 => Empty
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
  }

  def take(n: Int): Stream[A] = unfold((this,n)){
    case (Empty,_) => None
    case (_,0) => None
    case (Cons(h,t),m) => Some((h(),(t(),m-1)))
  }


  def drop(n: Int): Stream[A] = {

    @tailrec
    def loop(s: Stream[A], i: Int): Stream[A] = s match {
      case Cons(h, t) if i > 0 => loop(t(), i - 1)
      case _ => s
    }

    loop(this, n)
  }

  def takeWhileFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((elem,acc) => if(p(elem)) cons(elem,acc) else empty[A])

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h,t) if p(h()) => Some(h(),t())
    case _ => None
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((elem, b) => p(elem) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((elem,b) => p(elem) && b)

  def mapFoldRight[B](f: A => B): Stream[B] = foldRight(empty[B])((elem,acc) => cons(f(elem),acc))

  def map[B](f: A => B): Stream[B] = unfold(this){
    case Empty => None
    case Cons(h,t) => Some((f(h()),t()))
  }

  def filter(p: A => Boolean):Stream[A] = foldRight(empty[A])((elem,acc) => if(p(elem)) cons(elem,acc) else acc)

  def append[B >: A](s2: => Stream[B]):Stream[B] = foldRight(s2)((elem,acc) => cons(elem,acc))

  def flatMap[B](f: A => Stream[B]):Stream[B] = foldRight(empty[B])((elem,acc) => f(elem).append(acc))

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this,s2)){
    case (Empty,Empty) => None
    case (s1,s2) => Some((s1.headOption,s2.headOption),(s1.drop(1),s2.drop(1)))
  }

  def startsWith[B](s: Stream[B]): Boolean = this.zipAll(s).takeWhile(_._2.nonEmpty).forAll{
    case (e1,e2) => e1 == e2
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

  def constant[A](a: A): Stream[A] = unfold(a)(s => Some(s,s))

  def ones:Stream[Int] = constant(1)

  def from(n: Int): Stream[Int] = unfold(n)(s => Some(s,s+1))

  def fibs = cons(0,cons(1,unfold((0,1)){case (prev2,prev1) => Some(prev2+prev1, (prev1,prev2+prev1))}))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a,unfold(s)(f))
    case None => Empty
  }

  def zipWith[A,B,C](s1:Stream[A],s2:Stream[B])(f: (A,B) => C):Stream[C] = unfold((s1,s2)){
    case (Cons(h1,t1),Cons(h2,t2)) => Some(f(h1(),h2()),(t1(),t2()))
    case _ => None
  }
}