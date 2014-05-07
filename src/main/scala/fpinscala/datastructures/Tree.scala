package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree{

  def size[A](tree:Tree[A]):Int = {

    @tailrec
    def loop(t1:Tree[A], acc:(List[Tree[A]], Int)):Int = {
      (t1,acc) match {
        case (Leaf(_),(Nil,s)) => s + 1
        case (Leaf(_),(Cons(h,t),s)) => loop(h,(t,s+1))
        case (Branch(l,r),(pt,s)) => loop(l,(Cons(r,pt),s))
      }
    }

    loop(tree,(List(),0))

  }

  def max(tree:Tree[Int]):Int = {

    @tailrec
    def loop(t1:Tree[Int], acc:(List[Tree[Int]], Option[Int])):Int = {
      (t1,acc) match {
        case (Leaf(n),(Nil,m)) => n max m.getOrElse(n)
        case (Leaf(n),(Cons(h,t),m)) => loop(h,(t,Some(n max m.getOrElse(n))))
        case (Branch(l,r),(pt,m)) => loop(l,(Cons(r,pt),m))
      }
    }

    loop(tree,(List(),None))

  }
}