package fpinscala.state

import org.scalatest.FlatSpec


class RNGSpec extends FlatSpec {

  case class DummyRNG(seed:Int) extends RNG {
    override def nextInt: (Int, RNG) = (seed,DummyRNG(seed+1))
  }

  private def newRng(nextInt:Int) = DummyRNG(nextInt)

  behavior of "newRng"

  it should "generate a RNG that will deliver the given Int" in{
    assert(newRng(23).nextInt._1 === 23)
  }

  behavior of "nonNegativeInt"

  import RNG.nonNegativeInt

  it should "generate a non negative int for -1" in{
    assert(nonNegativeInt(newRng(-1))._1 === 0)
  }

  it should "generate a non negative int for 0" in{
    assert(nonNegativeInt(newRng(0))._1 === 0)
  }
  it should "generate a non negative int for Int.MinValue" in {
    assert(nonNegativeInt(newRng(Int.MinValue))._1 === Int.MaxValue)
  }

  it should "return the new state" in {
    val (nni1,rng) = nonNegativeInt(newRng(23))
    val (nni2,_) = nonNegativeInt(rng)
    assert(nni1 !== nni2)
  }

  behavior of "double"

  import RNG.double

  it should "generate a double up to 1" in {
    assert(double(newRng(Int.MaxValue))._1 < 1d)
  }

  it should "generate a double from 0" in {
    assert(double(newRng(0))._1 === 0d)
  }

  it should "generate double values, not only integer values" in {
    val res = double(newRng(23))._1
    assert(res > 0d)
    assert(res < 1d)
  }

  it should "return the new state" in {
    val (d1,rng) = double(newRng(23))
    val (d2,_) = double(rng)
    assert(d1 !== d2)
  }

  behavior of "intDouble"

  import RNG.intDouble
  import RNG.int

  it should "generate a pair of int,double with different values" in {
    val rng = newRng(23)
    val((i,d),rng3) = intDouble(rng)

    val (_,rng2) = int(rng)
    assert(i === int(rng)._1)
    assert(i !== int(rng2)._1)
    assert(i !== int(rng3)._1)
    assert(d === double(rng2)._1)
    assert(d !== double(rng)._1)
    assert(d !== double(rng3)._1)
  }

  behavior of "double3"

  import RNG.double3

  it should "generate different doubles" in {
    val rng = newRng(12)
    val ((d1,d2,d3),rng2) = double3(rng)
    val d4 = double(rng2)
    assert(d1 !== d2)
    assert(d1 !== d3)
    assert(d1 !== d4)
    assert(d2 !== d3)
    assert(d2 !== d4)
    assert(d3 !== d4)
  }

  behavior of "ints"

  import RNG.ints

  it should "generate a list of random ints" in {
    val (l,rng) = ints(3)(newRng(23))
    val (i3,_) = rng.nextInt
    assert(l === List(23,24,25))
    assert(i3 === 26)
  }

}
