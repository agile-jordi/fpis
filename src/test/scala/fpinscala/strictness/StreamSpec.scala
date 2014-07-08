package fpinscala.strictness

import org.scalatest.FlatSpec
import Stream._

class StreamSpec extends FlatSpec {

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def range(from:Int, to:Int): Stream[Int] = if(to < from) empty else cons(from, range(from + 1,to))

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

  it should "take some elements from an infinite stream" in {
    assert(from(23).take(3).toList === List(23, 24, 25))
  }

  behavior of "drop"

  it should "drop from the empty stream" in {
    assert(Stream().drop(3).toList === List.empty)
  }

  it should "drop from the non empty stream" in {
    assert(Stream(23, 42).drop(1).toList === List(42))
  }

  it should "drop from the non empty stream but only until it is empty" in {
    assert(Stream(23).drop(5).toList === List())
  }

  it should "drop from the infinite stream" in {
    assert(from(23).drop(5).take(3).toList === List(28, 29, 30))
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

  it should "take from the infinite stream while the predicate holds" in {
    assert(from(23).takeWhile(_ < 25).toList === List(23, 24))
  }

  behavior of "forall"

  it should "return true for the empty stream" in {
    assert(Stream().forAll(_ => false) === true)
  }

  it should "return true for some finite stream" in {
    assert(Stream(2,4,6).forAll(_ % 2 == 0) === true)
  }

  it should "return false failing fast" in {
    assert(from(23).forAll(_ < 30) === false)
  }

  behavior of "headOption"

  it should "return None for the empty stream" in {
    assert(Stream().headOption === None)
  }

  it should "return the head of a non empty stream" in{
    assert(from(23).headOption === Some(23))
  }

  behavior of "map"

  it should "map the empty stream" in {
    assert(Stream().map(_.toString).toList === List())
  }

  it should "map the infinite stream" in {
    assert(from(23).map(_ + 2).take(2).toList === List(25,26))
  }

  behavior of "filter"

  it should "filter the empty stream" in {
    assert(Stream[Int]().filter(_ > 24).toList === List())
  }

  it should "filter the infinite stream" in {
    assert(from(23).filter(_ % 2 == 0).take(2).toList === List(24,26))
  }

  behavior of "append"

  it should "append an empty stream to an empty stream" in {
    assert(Stream[Int]().append(Stream[Int]()).toList === List())
  }

  it should "append a non empty stream to an empty stream" in{
    assert(Stream[Int]().append(from(23)).take(2).toList === List(23,24))
  }

  it should "append a non empty stream to a non empty stream" in {
    assert(Stream(1,2,3).append(from(23)).take(5).toList === List(1,2,3,23,24))
  }

  it should "not evaluate the stream to append if not needed" in {
    assert(from(23).append(throw new RuntimeException("No!")).take(2).toList === List(23,24))
  }

  behavior of "flatMap"

  it should "flatMap over an empty sream" in {
    assert(Stream[Int]().flatMap(from).toList === List())
  }

  it should "flatMap over a non empty stream" in {
    assert(range(2,3).flatMap(i => range(i,i+2)).toList === List(2,3,4,3,4,5))
  }

  behavior of "constant"

  it should "well... be constant" in{
    assert(constant("a").drop(5).take(5).toList.mkString("") === "aaaaa")
  }

}
