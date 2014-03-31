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

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => throw new NoSuchElementException
    case Cons(x,xs) => xs
  }

  def setHead[A](list: List[A], h: A): List[A] = list match {
    case Nil => throw new NoSuchElementException
    case Cons(x,xs) => Cons(h,xs)
  }



}