package fpinscala

import org.scalatest.FlatSpec

class MyModuleSpec extends FlatSpec {

  behavior of "MyModule.fib"

  it should "calculate fib(0)" in {
    assert(MyModule.fib(0) === 0)
  }

  it should "calculate fib(1)" in {
    assert(MyModule.fib(1) === 1)
  }

  it should "calculate fib(n) where n > 1" in {
    assert(MyModule.fib(6) === 8) //0 1 1 2 3 5 8
  }

  behavior of "MyModule.isSorted"

  it should "return true for an empty array" in {
    assert(MyModule.isSorted[Int](Array.empty, _ > _) === true)
  }

  it should "return true for an array with only one element" in {
    assert(MyModule.isSorted[Int](Array(23), _ > _) === true)
  }

  it should "return true for a sorted array with N elements" in {
    assert(MyModule.isSorted[Int](Array(23, 42, 158), _ > _) === true)
  }

  it should "return false for a not sorted array with N elements" in {
    assert(MyModule.isSorted[Int](Array(42, 23), _ > _) === false)
  }

}
