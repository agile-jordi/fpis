package fpinscala.parallelism

class Par[A] {
}

object Par {
  def unit[A](a: => A): Par[A] = ???

  def get[A](a: Par[A]): A = ???

  def map2[A, B, C](par1: Par[A], par2: Par[B])(f: (A, B) => C): Par[C] = ???
}
