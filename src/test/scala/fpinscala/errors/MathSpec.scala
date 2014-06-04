package fpinscala.errors

import org.scalatest.FlatSpec

class MathSpec extends FlatSpec{

  import Math._

  behavior of "variance"

  it should "calculate the variance of an empty seq" in{
    assert(variance(Seq()) === None)
  }

  it should "calculate the variance of a seq with only one value" in{
    assert(variance(Seq(3.0)) === Some(0.0))
  }

  it should "calculate the variance of a non empty seq" in{
    assert(variance(Seq(2.0, 8.0)) === Some(9.0))
  }

  behavior of "mean"

  it should "calculate the mean of an empty seq" in {
    assert(mean(Seq()) === None)
  }

  it should "calculate the mean of a seq with only one value" in{
    assert(mean(Seq(1.3)) === Some(1.3))
  }

  it should "calculate the mean of a non empty seq" in {
    assert(mean(Seq(2.0,4.0)) === Some(3.0))
  }

}
