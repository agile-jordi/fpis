package fpinscala.errors

import scala.util.control.NonFatal

trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

object Option{
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case NonFatal(_) => None }
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(aa => b.map(bb => f(aa,bb)))
}

case class Some[A](v:A) extends Option[A] {

  override def map[B](f: (A) => B) = Some(f(v))

  override def flatMap[B](f: (A) => Option[B]) = f(v)

  override def filter(f: (A) => Boolean) = if(f(v)) this else None

  override def getOrElse[B >: A](default: => B) = v

  override def orElse[B >: A](ob: => Option[B]) = this
}

case object None extends Option[Nothing] {

  override def map[B](f: (Nothing) => B) = None

  override def flatMap[B](f: (Nothing) => Option[B]) = None

  override def filter(f: (Nothing) => Boolean) = None

  override def getOrElse[B >: Nothing](default: => B) = default

  override def orElse[B >: Nothing](ob: => Option[B]) = ob
}