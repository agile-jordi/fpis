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

  type Rand[+A] = State[RNG,A]

  val int: Rand[Int] = State(_.nextInt)

  def nonNegativeInt:Rand[Int] = int.map(i => if(i < 0) -(i+1) else i)

  val double: Rand[Double] = nonNegativeInt.map(_ / (Int.MaxValue.toDouble + 1))

  val intDouble: Rand[(Int,Double)] = State.both(int,double)

  val doubleInt:Rand[(Double,Int)] = State.both(double,int)

  val double3: Rand[(Double,Double,Double)] =
    double.flatMap(d1 => double.flatMap(d2 => double.map(d3 => (d1,d2,d3))))

  def ints(count: Int): Rand[List[Int]] = State.sequence(List.fill(count)(int))

  def nonNegativeEven: Rand[Int] =
    nonNegativeInt.map(i => i - i % 2)

  def nonNegativeLessThan(n: Int): Rand[Int] = nonNegativeInt.flatMap { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) State.unit(mod) else nonNegativeLessThan(n)
  }

  def rollDie: Rand[Int] = nonNegativeLessThan(6).map(_ + 1)
}



