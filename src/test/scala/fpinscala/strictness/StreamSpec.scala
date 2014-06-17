package fpinscala.strictness

import org.scalatest.FlatSpec
import Stream._

class StreamSpec extends FlatSpec {

  behavior of "toList"

  it should "return the empty list for an empty stream" in {
    assert(Stream().toList === List.empty)
  }

  it should "convert a single element stream into a list" in {
    assert(Stream(23).toList === List(23))
  }

  it should "convert a multiple element stream into a list" in {
    assert(Stream(23, 42).toList === List(23, 42))

  }

  behavior of "take"

  it should "return the empty list for the empty stream" in {
    assert(Stream().take(2).toList === List())
  }

  it should "return the first element of a non empty stream" in {
    assert(Stream(23).take(1).toList === List(23))
  }

  it should "return the first elements if we ask more than it has" in {
    assert(Stream(23, 42).take(2).toList === List(23, 42))
  }

  behavior of "drop"

  it should "drop from the empty stream" in {
    assert(Stream().drop(3).toList === List.empty)
  }

  it should "drop from the non empty stream" in {
    assert(Stream(23, 42).drop(1).toList === List(42))
  }

  it should "dr8op from the non empty stream but only until it is empty" in {
    assert(Stream(23).drop(5).toList === List())
  }

  behavior of "takeWhile"

  it should "take from the empty stream" in {
    assert(Stream[Int]().takeWhile(_ => true).toList === List.empty)
  }

  it should "take from the non empty stream, taking no elements" in {
    assert(Stream(23).takeWhile(_ => false).toList === List())
  }

  it should "take from the non empty stream while the predicate holds" in {
    assert(Stream(2, 4, 5).takeWhile(_ % 2 == 0).toList === List(2, 4))
  }

}
