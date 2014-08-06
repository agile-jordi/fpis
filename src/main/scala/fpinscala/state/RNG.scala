package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG{

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a,rng2) = ra(rng)
    val (b,rng3) = rb(rng2)
    (f(a,b),rng3)
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(unit(List.empty[A])) { (acc, f) =>
      map2(f, acc)(_ :: _)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (res,nextRng) = rng.nextInt
    (if(res < 0) -(res+1) else res,nextRng)
  }

  val double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  val intDouble: Rand[(Int,Double)] = both(int,double)

  val doubleInt:Rand[(Double,Int)] = both(double,int)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,rng2) = double(rng)
    val (d2,rng3) = double(rng2)
    val (d3,rng4) = double(rng3)
    ((d1,d2,d3),rng4)
  }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)
}



