package fpinscala.datastructures

import org.scalatest.FlatSpec

class ListSpec extends FlatSpec {

  behavior of "pattern matching"

  it should "pattern match on liss" in {

    val l = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }

    assert(l === 3)

  }

  behavior of "List.tail"

  it should "fail on empty lists" in{
    intercept[NoSuchElementException]{
      List.tail(List())
    }
  }

  it should "return the tail of a non empty list" in {
    assert(List.tail(List(1,2,3)) === List(2,3))
  }

}
