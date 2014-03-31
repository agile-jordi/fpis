package fpinscala

import org.scalatest.FlatSpec

class MyModuleSpec extends FlatSpec{

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

}
