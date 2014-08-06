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

}
