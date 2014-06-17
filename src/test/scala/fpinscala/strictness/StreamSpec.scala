package fpinscala.strictness

import org.scalatest.FlatSpec
import Stream._

class StreamSpec extends FlatSpec {

  behavior of "toList"

  it should "return the empty list for an empty stream" in {
    assert(empty.toList === List.empty)
  }

  it should "convert a single element stream into a list" in {
    assert(cons(23, empty).toList === List(23))
  }

  it should "convert a multiple element stream into a list" in {
    assert(cons(23, cons(42,empty)).toList === List(23,42))

  }

  behavior of "take"

  it should "return the empty list for the empty stream" in {
    assert(empty.take(2) === List())
  }

  it should "return the first element of a non empty stream" in {
    assert(cons(23, empty).take(1) === List(23))
  }

  it should "return the first elements if we ask more than it has" in {
    assert(cons(23, cons(42, empty)).take(2) === List(23, 42))
  }

  behavior of "drop"

  it should "drop from the empty stream" in {
    assert(empty.drop(3).toList === List.empty)
  }

  it should "drop from the non empty stream" in {
    assert(cons(23, cons(43, empty)).drop(1).toList === List(43))
  }

  it should "dr8op from the non empty stream but only until it is empty" in {
    assert(cons(23, empty).drop(5).toList === List())
  }

  behavior of "takeWhile"

  it should "take from the empty stream" in {
    assert(empty[Int].takeWhile(_ => true).toList === List.empty)
  }

  it should "take from the non empty stream, taking no elements" in {
    assert(cons(23,empty).takeWhile(_ => false).toList === List())
  }

  it should "take from the non empty stream while the predicate holds" in {
    assert(cons(2,cons(4,cons(5,empty))).takeWhile(_ % 2 == 0).toList === List(2,4))
  }

}
