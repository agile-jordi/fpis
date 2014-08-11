package fpinscala.state

import org.scalatest.FlatSpec

class MachineSpec extends FlatSpec {

  behavior of "Machine"

  import Machine._

  it should "unlock when inserting a coin if there is any candy left" in {
    val m = Machine(locked = true, candies = 3, coins = 4)
    val simulation = simulateMachine(List(Coin))
    val (res, m2) = simulation(m)
    assert(m2 === Machine(locked = false, candies = 3, coins = 4 + 1))
    assert(res._1 === m2.candies)
    assert(res._2 === m2.coins)
  }

  it should "dispense candy when turning knob if unlocked" in {
    val m = Machine(locked = false, candies = 3, coins = 5)
    val (res, m2) = simulateMachine(List(Turn))(m)
    assert(m2 === Machine(locked = true, candies = 2, coins = 5))
    assert(res._1 === m2.candies)
    assert(res._2 === m2.coins)
  }

  it should "satisfy that turning the knob on a locked machine does nothing" in {
    val m = Machine(locked = true, candies = 3, coins = 5)
    val (res, m2) = simulateMachine(List(Turn))(m)
    assert(m2 === m)
    assert(res._1 === m2.candies)
    assert(res._2 === m2.coins)
  }

  it should "satisfy that inserting a coin into an unlocked machine does nothing" in {
    val m = Machine(locked = false, candies = 3, coins = 5)
    val (res, m2) = simulateMachine(List(Coin))(m)
    assert(m2 === m)
    assert(res._1 === m2.candies)
    assert(res._2 === m2.coins)
  }

  it should "ignore inputs if out of candy when locked" in {
    val m = Machine(locked = true, candies = 0, coins = 5)
    val (res, m2) = simulateMachine(List(Coin, Turn))(m)
    assert(m2 === m)
    assert(res._1 === m2.candies)
    assert(res._2 === m2.coins)
  }

  it should "ignore inputs if out of candy when unlocked" in {
    val m = Machine(locked = false, candies = 0, coins = 5)
    val (res, m2) = simulateMachine(List(Coin, Turn))(m)
    assert(m2 === m)
    assert(res._1 === m2.candies)
    assert(res._2 === m2.coins)
  }
}
