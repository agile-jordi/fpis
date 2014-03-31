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

  def dropWhile[A](l: List[A], f: A => Boolean):List[A] = l match {
    case Cons(x,xs) if f(x) => dropWhile(xs,f)
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

}