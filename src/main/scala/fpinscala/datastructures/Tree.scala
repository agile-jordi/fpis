package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{

  def foldLeft[A,B](tree:Tree[A], z:B)(f: (A,B) => B):B = {
    @tailrec
    def loop(t1:Tree[A], acc:(List[Tree[A]], B)):B = {
      (t1,acc) match {
        case (Leaf(n),(Nil,b)) => f(n,b)
        case (Leaf(n),(Cons(h,t),b)) => loop(h,(t,f(n,b)))
        case (Branch(l,r),(pt,b)) => loop(l,(Cons(r,pt),b))
      }
    }

    loop(tree,(List(),z))
  }

  def fold[A,B](tree:Tree[A])(z:(A) => B)(f:(B,B)=>B):B = tree match {
    case Leaf(a) => z(a)
    case Branch(l,r) => f(fold(l)(z)(f),fold(r)(z)(f))
  }

  def size[A](tree:Tree[A]):Int = fold(tree)(_ => 1)(_ + _)

  def max(tree:Tree[Int]):Int = fold(tree)(l => l)(_ max _)

  def depth[A](tree:Tree[A]):Int = fold[A,Int](tree)(_ => 1)((l,r) => (l max r) + 1)
}
