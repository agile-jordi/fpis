package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{

  def fold[A,B](tree:Tree[A], z:B)(f: (A,B) => B):B = {
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

  def size[A](tree:Tree[A]):Int = fold(tree,0)((_,acc) => acc+1)

  def max(tree:Tree[Int]):Int = fold[Int,Option[Int]](tree,None)((e,acc) => Some(e max acc.getOrElse(e))).get

  def depth[A](tree:Tree[A]):Int = tree match{
    case Leaf(_) => 1
    case Branch(l,r) => (depth(l) max depth(r)) + 1
  }
}
