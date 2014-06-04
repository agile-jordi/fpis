package fpinscala.errors

import org.scalatest.FlatSpec

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


}
