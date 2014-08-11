package fpinscala.state

case class State[S,+A](run: S => (A,S)) {
  def apply(s:S) = run(s)
  def map[B](f: A => B): State[S,B] = this.flatMap(a => State.unit(f(a)))
  def flatMap[B](g: A => State[S,B]): State[S,B] = State(s => {
    val (a,s2) = this(s)
    g(a)(s2)
  })
}

object State{
  def unit[S,A](a: A): State[S,A] = State[S,A](s => (a,s))
  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.foldLeft(unit[S,List[A]](List.empty)) { (acc, f) =>
      map2(f,acc)(_ :: _)
    }
  def map2[S,A,B,C](sa:State[S,A], sb: State[S,B])(f: (A, B) => C): State[S,C] = sa.flatMap(a => sb.map(b => f(a,b)))
  def both[S,A,B](sa:State[S,A],sb: State[S,B]): State[S,(A,B)] = map2(sa,sb)((_, _))
}
