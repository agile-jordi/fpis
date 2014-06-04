package fpinscala.errors

import org.scalatest.FlatSpec
import fpinscala.datastructures.List

class OptionSpec extends FlatSpec{

  behavior of "option"

  private val sayHello: (String) => String = s => s"Hello $s"


  it should "map over Some(v)" in{
    assert(Some("World").map(sayHello) === Some("Hello World"))
  }

  it should "map over None" in {
    assert(None.map(sayHello) === None)
  }

  it should "flatMap over Some(v)" in {
    assert(Some("World").flatMap(s => Some(s"Hello $s")) === Some("Hello World"))
  }

  it should "flatMap over None" in{
    assert(None.flatMap(s => Some(s"Hello $s")) === None)
  }

  it should "getOrElse over Some(v)" in {
    assert(Some("World").getOrElse("Void") === "World")
  }

  it should "getOrElse over None" in{
    assert(None.getOrElse("World") === "World")
  }

  it should "orElse over Some(v)" in{
    assert(Some("World").orElse(Some("Void")) === Some("World"))
  }

  it should "orElse over None" in{
    assert(None.orElse(Some("World")) === Some("World"))
  }

  behavior of "map2"

  import Option.map2

  private val concat: (String,String) => String = _ + _

  it should "map over no values" in {
    assert(map2(None,None)(concat) === None)
  }

  it should "map over just one value" in {
    assert(map2(None,Some("World"))(concat) === None)
    assert(map2(Some("Hello"),None)(concat) === None)
  }

  it should "map over two values" in{
    assert(map2(Some("Hello "),Some("World"))(concat) === Some("Hello World"))
  }

  behavior of "sequence"

  import Option.sequence

  it should "return the empty list for the empty list" in{
    assert(sequence(List.apply[Option[String]]()) === Some(List()))
  }

  it should "return None if the list contains a None" in {
    assert(sequence(List(Some(1),Some(2), None, Some(3))) === None)
  }

  it should "return the list if all the elements in the list are defined" in {
    assert(sequence(List(Some(1),Some(2), Some(3))) === Some(List(1,2,3)))
  }

  behavior of "traverse"

  import Option.traverse

  it should "return the empty list for the empty list" in {
    assert(traverse(List.apply[String]())(_ => Some("Yay!")) === Some(List()))
  }

  private val evenMinusOne = (i:Int) => if (i%2 == 0) Some(i-1) else None

  it should "traverse a non empty list without errors" in {
    assert(traverse(List(2,4,6))(evenMinusOne) === Some(List(1,3,5)))
  }

  it should "return none if a single element returns none" in {
    assert(traverse(List(2,5,6))(evenMinusOne) === None)
  }

  behavior of "sequence in terms of traverse"

  import Option.sequenceInTermsOfTraverse

  it should "return the empty list for the empty list" in{
    assert(sequenceInTermsOfTraverse(List.apply[Option[String]]()) === Some(List()))
  }

  it should "return None if the list contains a None" in {
    assert(sequenceInTermsOfTraverse(List(Some(1),Some(2), None, Some(3))) === None)
  }

  it should "return the list if all the elements in the list are defined" in {
    assert(sequenceInTermsOfTraverse(List(Some(1),Some(2), Some(3))) === Some(List(1,2,3)))
  }



}
