package fpinscala.strictness

import org.scalatest.FlatSpec
import Stream._

class StreamSpec extends FlatSpec{

  behavior of "toList"

  it should "return the empty list for an empty stream" in {
    assert(empty.toList === List.empty)
  }

  it should "convert a single element stream into a list" in{
    assert(cons(23,empty).toList === List(23))
  }

}
