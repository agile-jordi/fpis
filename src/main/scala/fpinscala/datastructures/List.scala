package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(i, xs) => i + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](elems: A*): List[A] =
    if (elems.isEmpty) Nil
    else Cons(elems.head, apply(elems.tail: _*))

  def tail[A](list: List[A]): List[A] = List.drop(list,1)

  def setHead[A](list: List[A], h: A): List[A] = list match {
    case Nil => throw new NoSuchElementException
    case Cons(x,xs) => Cons(h,xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    require(n >= 0)
    l match {
      case _ if n == 0 => l
      case Nil => throw new NoSuchElementException
      case Cons(x,xs) => drop(xs,n-1)
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if f(x) => dropWhile(xs)(f)
      case _ => l
    }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new NoSuchElementException
    case Cons(x,Nil) => Nil
    case Cons(x,xs) => Cons(x,init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _)

  def shortFoldRight[A, B](as: List[A], z: B, shortcut:B)(f: (A, B) => B): (B,Int) =
    as match {
      case Nil => (z,0)
      case Cons(x, xs) if x == shortcut  => (shortcut,0)
      case Cons(x, xs) =>
        val (res, count) = shortFoldRight(xs,z,shortcut)(f)
        (f(x, res), count + 1)
    }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs,f(z,x))(f)
    }
  }

  def sumFoldLeft(ns:List[Int]):Int = List.foldLeft(ns,0)(_ + _)

  def productFoldLeft(ds: List[Double]): Double = List.foldLeft(ds,1.0)(_ * _)

  def lengthFoldLeft[A](l: List[A]): Int = List.foldLeft(l,0)((acc,_) => acc + 1)

  def reverse[A](l:List[A]):List[A] = List.foldLeft(l,List[A]())((acc,e) => Cons(e,acc))

  // Had to copy the solution for this one...
  def foldLeftInTermsOfFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    // foldRight(List(1,2),(b:B) => B)(...) === foldRight(List(1),b:B => f(b,2))(...) == foldRight(List(), b:B => f(f(b,1),2))(...)
    List.foldRight(l,(b:B) => b)((e,acc) => b => acc(f(b,e)))(z)
  }

  def foldRightInTermsOfFoldLeft[A,B](l:List[A], z:B)(f:(A,B) => B):B = {
    List.foldLeft(l, (b:B) => b)((acc,e) => b => acc(f(e,b)))(z)
  }

  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] = {
    List.foldRightInTermsOfFoldLeft(a1,a2)((e,acc) => Cons(e,acc))
  }

  def concat[A](ll: List[List[A]]):List[A] = List.foldLeft(ll,List[A]())((acc,e) => List.appendViaFold(acc,e))

  def map[A, B](l: List[A])(f: A => B): List[B] = List.foldRight(l, List[B]())((e, acc) => Cons(f(e), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = List.foldRight(l, List[A]())((e,acc) => if(f(e)) Cons(e,acc) else acc)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = List.foldLeft(l, List[B]())((acc,e) => List.append(acc,f(e)))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = List.flatMap(l)(e => if(f(e)) List(e) else List())

  def add(l1:List[Int], l2:List[Int]):List[Int] = {

    def loop(l1:List[Int], l2:List[Int]):List[Int] = {
      (l1, l2) match {
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2,loop(t1, t2))
        case _ => Nil
      }
    }
    loop(l1,l2)
  }
}